# 01_build_dataset.R
# Builds the monument-level dataset: removal status, state-level CRT index, county government composition,
# and neighbourhood covariates for a 33.8 km buffer around each monument (area-weighted from county data).
# Output: model_data.csv, the input of 02_models.R

library("sf")       #spatial data: points, polygons, buffers, intersections
library("reshape2") #reshaping between long and wide formats
library("dplyr")    #joins and data frame manipulation
library("ggplot2")  #plotting
library("readxl")   #reading the SPLC Excel file
library("stringr")  #string manipulation (splitting the coordinate strings)

# ---- 1. State-level CRT index ----

CRT<-read.csv("CRT.csv") #Read in the CRT Forward database of measures restricting Critical Race Theory in education
CRT$Index<-apply(CRT[,14:32],1,sum) #Sum the restriction indicator columns: the number of restrictions in each measure

state<-CRT[CRT$geography_type=="state",c(6,42)] #Keep state-level measures only, with the state code and the index
state<-aggregate(Index~state_us,state,sum) #Sum the index over all measures of a state
colnames(state)<-c("st","CRTINDEX") #Rename the columns so they can be joined to the monuments by state code
remove(CRT) #Remove the original data frame

# ---- 2. Monuments and removal status ----

WHSF<-read_xlsx("WHSF.xlsx",sheet=2) #Read in the SPLC "Whose Heritage?" database of Confederate monuments
WHSF<-WHSF[!duplicated(WHSF[,1]),] #Drop duplicated monument IDs
WHSF[,1]<-as.character(WHSF$`Unique ID`) #Store the monument ID as character, so it joins consistently later

y<-WHSF[,c(14,13,1,6)] #Keep the removal status, the year of removal, the monument ID and the state code
colnames(y)<-c("active","year_remove","id","st") #Rename the columns
y<-y[!is.na(y$active),] #Drop monuments whose status is unknown
y$year_remove<-ifelse(y$active=="Live",Inf,y$year_remove) #Monuments still standing ("Live") get an infinite removal year, so they survive the filter below
y$year_remove<-ifelse(is.na(y$year_remove),2024,y$year_remove) #Pending removals without a year are set to 2024
y<-y[y$year_remove>2015,] #Drop monuments removed before 2016: the analysis covers removals from 2016 onwards
y<-y[,c(1,3:4)] #Drop the year column, it is no longer needed

y<-left_join(y,state) #Attach the CRT index of the monument's state
y$CRTINDEX<-ifelse(is.na(y$CRTINDEX),0,y$CRTINDEX) #States without any recorded measure get an index of 0
remove(state) #Remove the state table

y$active<-ifelse(y$active=="Live",0,1) #Outcome variable: 1 if the monument is no longer standing (removed or pending), 0 if it is still standing
y<-na.omit(y) #Drop any remaining incomplete rows

# ---- 3. County of each monument and county government composition ----

cnty<-WHSF[,c(1,8)] #Keep the monument ID and its coordinate string ("lat, lon")
cnty<-data.frame(id=cnty$`Unique ID`,C=apply(str_split_fixed(cnty$Coordinates,",",2),2,as.numeric)) #Split the string into two numeric columns: C.1 latitude, C.2 longitude
cnty<-na.omit(cnty) #Drop monuments without coordinates
cnty<-st_as_sf(cnty,coords=c("C.2","C.1"),crs=4326) #Turn them into sf points (longitude first, then latitude; WGS84)

map<-read_sf("counties.geojson") #Read in the map of current US counties
map<-map[!map$STATEFP%in%c("02",NA,"15","72"),5] #Keep the contiguous US only (drop Alaska, Hawaii and Puerto Rico) and the GEOID column
map<-map%>%st_transform(crs=4326) #Use the same coordinate system as the monuments
map<-st_simplify(map,preserveTopology = FALSE, dTolerance = 5000) #Simplify the county shapes (5 km tolerance) to speed up the intersections
map$A<-st_area(map) #Area of each county, needed for the area weights in section 4
ggplot(map)+geom_sf() #Plot the county map as a visual check

wat<-st_intersects(cnty,map) #Find the county that contains each monument
wat<-lapply(wat,function(x)ifelse(length(x)==1,x,NA)) #If a monument does not fall into exactly one county, set it to NA
wat<-unlist(wat) #Turn the list into a vector of county row numbers
cnty$GEOID<-map$GEOID[c(wat)] #Look up the county GEOID (FIPS code) of each monument
cnty$GEOID<-as.numeric(cnty$GEOID) #Store GEOID as a number, as in the county government file

locgov<-read.csv("CNTY.csv") #Read in our county government file: party and racial composition of the county executive (built from NACo and Reflective Democracy data with our weighting scheme)
colnames(locgov)[1]<-"GEOID" #Rename the FIPS column to GEOID for the join

cnty<-left_join(cnty,locgov)%>%st_drop_geometry() #Attach the county government variables to each monument and drop the point geometry
cnty<-cnty[,c(1,3:5)] #Keep the monument ID and the county government variables

y<-left_join(y,cnty) #Attach the county government variables to the outcome table

# ---- 4. Monument neighbourhoods: 33.8 km buffers and area weights ----

cnty<-WHSF[,c(1,8)] #Take the monument IDs and coordinates again
cnty<-data.frame(id=cnty$`Unique ID`,C=apply(str_split_fixed(cnty$Coordinates,",",2),2,as.numeric)) #Split the coordinates as in section 3
cnty<-na.omit(cnty) #Drop monuments without coordinates
cnty<-st_as_sf(cnty,coords=c("C.2","C.1"),crs=4326) #Convert to sf points
cnty<-st_buffer(cnty,33800) #Draw a circle with a 33.8 km radius around each monument: this is the monument's neighbourhood (half the average daily travel distance of US residents)

ST<-st_intersects(cnty,map) #For each neighbourhood, list the counties it touches

A<-list() #Empty list for the overlap shapes

for(I in 1:nrow(cnty)){
A[[I]]<-st_intersection(map[ST[[I]],],cnty[I,]) #Cut out the part of each touched county that lies inside the neighbourhood
print(I) #Progress counter
} #This loop is slow: one intersection per monument

B<-bind_rows(A) #Stack the overlap pieces into one table (one row per monument-county pair)
B$IA<-st_area(B) #Area of each overlap piece
B<-B%>%st_drop_geometry() #Drop the geometry, only the areas are needed from here
B$w<-as.numeric(B$IA/B$A) #Weight: the share of the county's area that falls inside the neighbourhood
B<-B[,c(3,1,5)] #Keep the monument ID, the county GEOID and the weight

remove(locgov,ST,WHSF,A,cnty,I,wat,map) #Remove objects that are no longer needed

# ---- 5. County data interpolated to the neighbourhoods ----

csv<-read.csv("demography.csv") #Read in the NHGIS county demographic data
csv$GEOID<-paste(substr(csv$GISJOIN,2,3),substr(csv$GISJOIN,5,7),sep="") #Build the GEOID (state + county FIPS) from the NHGIS GISJOIN code
csv<-csv[,c(117,43:78)] #Keep the GEOID and the population, race, education and income columns
csv$uni<-apply(csv[28:36],1,sum) #Sum the education categories above secondary level: adults with tertiary education
csv<-csv[,c(1,2,4,12,38,37)] #Keep GEOID, total population, Black population, adult population, tertiary educated and total income
colnames(csv)<-c("GEOID","POP","BLC","ADL","UNI","INC") #Rename the columns

load("elec.Rdata") #Read in the county presidential election returns (Algara & Amlani)
elec<-pres_elections_release #Copy it to a shorter name
elec<-elec[elec[,1]==2020,c(2,9,14)] #Keep the 2020 election: county ID, Democratic votes and total votes
remove(pres_elections_release) #Remove the original object
colnames(elec)<-c("GEOID","DEM","TOT") #Rename the columns

J<-left_join(B,csv) #Attach the county demographics to every monument-county pair
J<-left_join(J,elec) #Attach the county election results as well
J[,4:ncol(J)]<-apply(J[,4:ncol(J)],2,function(x)x*J$w) #Multiply every county count by the weight: the part of the county's population (votes, etc.) living inside the neighbourhood, assuming even spread within the county

remove(elec,csv) #Remove objects that are no longer needed

J<-aggregate(.~id,J[,c(1,4:ncol(J))],sum) #Sum the weighted counts over the counties of each neighbourhood: one row per monument

res<-left_join(y,J) #Attach the neighbourhood counts to the outcome table
res$rB<-res$BLC/res$POP #Share of Black residents in the neighbourhood
res$rU<-res$UNI/res$ADL #Share of adults with tertiary education in the neighbourhood
res$Den<-log(res$POP) #Log population density (every neighbourhood has the same area, so this is the log of population)
res$rD<-res$DEM/res$TOT #Democratic vote share in the neighbourhood, 2020
res$rINC<-res$INC/res$ADL/1000 #Income per adult in the neighbourhood, in thousand USD

X<-res[,c(1,4,12:ncol(res))] #Intermediate selection (X is rebuilt from res in section 6, after the protest counts are added)

# ---- 6. Black Lives Matter protests in the neighbourhoods ----

xlsx<-read_xlsx("WHSF.xlsx",sheet=2) #Read the monument file again
cnty<-xlsx[,c(1,8)] #Keep the monument ID and coordinates
cnty<-data.frame(id=cnty$`Unique ID`%>%as.character(),C=apply(str_split_fixed(cnty$Coordinates,",",2),2,as.numeric)) #Split the coordinates, ID as character
cnty<-na.omit(cnty) #Drop monuments without coordinates
cnty<-cnty[!duplicated(cnty$id),] #Drop duplicated IDs
cnty<-st_as_sf(cnty,coords=c("C.2","C.1"),crs=4326) #Convert to sf points
cnty<-st_buffer(cnty,33800) #Rebuild the 33.8 km neighbourhoods

remove(xlsx) #Remove the raw file

crisis<-read.csv("crisis.csv") #Read in the US protest events (ACLED)
crisis<-st_as_sf(crisis,coords=c("lat","lon"),crs=4326) #Convert the protest coordinates into sf points
crisis<-crisis[grepl("Black Lives Matter",crisis$party),] #Keep the events where Black Lives Matter is listed as an actor

count<-st_intersects(cnty,crisis) #For each monument neighbourhood, find the BLM events inside it
count<-data.frame(id=cnty$id,BLM=lengths(count)) #Number of BLM events per neighbourhood

X<-left_join(res,count) #Join the protest counts to the neighbourhood data

# ---- 7. Slavery in 1860, interpolated to the neighbourhoods ----

slavery<-read.csv("slavery.csv") #Read in the 1860 census county counts (NHGIS)
slavery<-slavery[,c(1,10,11)] #Keep the 1860 county identifier and the two population counts (enslaved: AHB001, and free)

MAP1<-st_read("US_county_1860.shp") #Read in the county boundaries of 1860 (NHGIS)
MAP1<-st_simplify(MAP1,dTolerance=5000) #Simplify the shapes (5 km tolerance) to speed up the intersections
MAP1<-MAP1[17] #Keep the 1860 county identifier (and the geometry)
MAP1<-st_transform(MAP1,crs=4326) #Use the same coordinate system as the neighbourhoods
MAP1$A<-st_area(MAP1) #Area of each 1860 county

ST<-st_intersects(cnty,MAP1) #For each neighbourhood, list the 1860 counties it touches

A<-list() #Empty list for the overlap shapes
for(I in 1:nrow(cnty)){
  A[[I]]<-st_intersection(MAP1[ST[[I]],],cnty[I,]) #Cut out the part of each touched 1860 county that lies inside the neighbourhood
  print(I) #Progress counter
}

B<-bind_rows(A) #Stack the overlap pieces into one table
B$IA<-st_area(B) #Area of each overlap piece
B$w<-as.numeric(B$IA/B$A) #Weight: the share of the 1860 county's area inside the neighbourhood
B<-B[,c(3,1,6)]%>%st_drop_geometry() #Keep the monument ID, the 1860 county identifier and the weight

B<-left_join(B,slavery) #Attach the 1860 population counts
B[,4:5]<-apply(B[,4:5],2,function(x)ifelse(is.na(x),0,x)) #Areas with no 1860 census record (e.g. unorganised territories) get zero population
B[,4:ncol(B)]<-apply(B[,4:ncol(B)],2,function(x)x*B$w) #Weight the 1860 counts by the overlap share, as in section 5
B<-aggregate(.~id,B[,c(1,4:ncol(B))],sum) #Sum over the 1860 counties of each neighbourhood

X<-left_join(X,B) #Attach the 1860 counts to the monument table

X$rSLV<-X$AHB001/apply(X[,18:19],1,sum) #Slavery intensity: enslaved population divided by total (enslaved + free) population in 1860
X$rSLV<-ifelse(is.nan(X$rSLV),0,X$rSLV) #Neighbourhoods with no 1860 population get an intensity of 0

# ---- 8. Final modelling dataset ----

X<-X[,c(1,2,4,5:7,15:20,23)] #Keep the outcome, the monument ID and the model variables (column positions follow the joins above)
X$BLM<-ifelse(is.na(X$BLM),0,X$BLM) #Neighbourhoods without any recorded BLM event get 0
X<-na.omit(X) #Drop monuments with missing data

X$locDEM<-ifelse(X$locDEM=="Split"|X$locDEM=="Yes",1,0) #Democratic county government: 1 if Democrats hold or share control, 0 otherwise
X$locBLC<-ifelse(X$locBLC=="Yes",1,0) #Black majority in county government: 1 if yes, 0 otherwise

write.csv(X,"model_data.csv",row.names=F) #Final modelling dataset, one row per monument
