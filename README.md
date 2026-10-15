# Who decides the past? Confederate monument removals in the US

Why were some Confederate monuments in the United States removed after 2015, while others are still standing? This repository contains the R code for a monument-level study of that question. It links each monument's removal status to the demographic, historical, political and institutional characteristics of its surroundings.

The paper (in Hungarian, with Noémi Imre) won second place in the political science section of the Corvinus TDK (Scientific Students' Associations Conference), 2025.

## Data

| Source | Used for | Level |
|---|---|---|
| SPLC *Whose Heritage?* | Monument locations and removal status (2,544 monuments) | point |
| NHGIS (IPUMS) | Population, Black population, education, income; 1860 census (free and enslaved population) and 1860 county boundaries | county |
| Algara & Amlani, county presidential returns | 2020 Democratic vote share | county |
| NACo and Reflective Democracy Campaign | Party and race of county executive bodies | county |
| CRT Forward | Measures restricting the teaching of Critical Race Theory | state |
| ACLED | Black Lives Matter protest events | point |

The raw data are not included, because most of these sources do not allow redistribution. They can be downloaded from the providers above.

## Method

**Monument neighbourhoods.** Assigning every monument the values of the county it stands in ignores neighbouring counties and gives identical values to monuments in the same county. Instead, each monument gets a 33.8 km buffer (half the average daily travel distance of US residents). County counts are interpolated onto the buffer by area: each county contributes in proportion to the share of its area that falls inside the buffer. Shares such as the Black population share or the Democratic vote share are then computed from the interpolated counts.

**Historical slavery.** The 1860 census counts of free and enslaved people are interpolated onto the same buffers, using the 1860 county boundaries.

**County government.** US counties are run by different combinations of commissions, administrators and elected executives. A weighting scheme based on which offices a county has, and which of them holds executive power, makes the party and racial composition of county government comparable across counties.

**Models.** Logistic regression of removal (1 = removed or removal pending since 2016, 0 = still standing):

| Model | Predictors |
|---|---|
| 1 | All variables |
| 2 | Model 1 without the variables that were not significant (education, Black county government, BLM protests) |
| 3 | Model 2 with slavery intensity × CRT index in place of the two separate variables |
| 4 | Model 2 with Black population share × CRT index in place of slavery intensity and the CRT index |

The models are compared by AIC and McFadden's pseudo-R². Model 3 has the lowest AIC; it is evaluated with a ROC curve, and its fitted probabilities are used to project expected removal years under a constant yearly hazard.

## Main results

Removal followed political representation rather than historical grievance. Democratic vote share, population density and Democratic county government predicted removal. Monuments in areas with a larger Black population share and higher 1860 slavery intensity were less likely to be removed (models 1–2). Model 3 has McFadden's R² = 0.14 and AUC = 0.75.

## Scripts

| Script | What it does | Output |
|---|---|---|
| `01_build_dataset.R` | Builds the 33.8 km buffers, interpolates the county and 1860 data onto them, and adds the CRT index, BLM protest counts and county government variables | `model_data.csv` |
| `02_models.R` | Fits the four models, computes the fit statistics, plots the ROC curve and the coefficients of model 3, projects expected removal years and reports classification accuracy | `model_comparasion.html`, plots |

### Input files

`01_build_dataset.R` expects these files in the working directory:

| File | Content |
|---|---|
| `WHSF.xlsx` | SPLC *Whose Heritage?* database (sheet 2) |
| `CRT.csv` | CRT Forward measures |
| `counties.geojson` | Current US county boundaries |
| `CNTY.csv` | County government composition, coded by the authors |
| `demography.csv` | NHGIS county demographics |
| `elec.Rdata` | Algara & Amlani county presidential returns |
| `crisis.csv` | ACLED protest events |
| `slavery.csv` | NHGIS 1860 census, free and enslaved population by county |
| `US_county_1860.shp` | NHGIS 1860 county boundaries |

R packages: `sf`, `dplyr`, `reshape2`, `ggplot2`, `readxl`, `stringr`, `pROC`, `stargazer`, `DescTools`, `lmtest`.

The two intersection loops in `01_build_dataset.R` run one spatial intersection per monument, so building the dataset takes a while.

## Known issues

- Models 3 and 4 leave out the main effects of the CRT index (and, in model 3, of slavery intensity), so the product terms are not full interactions.
- Pending removals are counted as removed.
- The analysis is cross-sectional, so the coefficients describe associations, not causal effects.

## Authors

Noémi Imre and Kolos Mohácsi
