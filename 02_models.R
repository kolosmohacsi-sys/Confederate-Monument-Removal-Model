# 02_models.R
# Logistic regression models of monument removal (2016 onwards), fit statistics, ROC curve,
# coefficient plot and a simple projection of expected removal years.

library("dplyr")     #data frame manipulation
library("ggplot2")   #plotting
library("pROC")      #ROC curve, AUC and optimal threshold
library("stargazer") #regression comparison table
library("DescTools") #McFadden's pseudo R-squared
library("lmtest")    #coefficient tests (standard errors for the coefficient plot)

X<-read.csv("model_data.csv") #Read in the monument-level dataset built by 01_build_dataset.R

# ---- 1. Models ----
# Outcome: active = 1 if the monument was removed (or its removal is pending), 0 if it still stands.
# In the formulas "." means every column of X; the "-" terms remove columns from the model.
# id (monument ID) and rule (county government autonomy) are never used as predictors,
# and rINC (income per adult) is left out of every model.

model1<-glm(active~.-id-rule-rINC,data=X,family="binomial") #Model 1: all predictors
summary(model1) #Coefficients, standard errors and AIC

model2<-glm(active~.-id-rU-locBLC-BLM-rINC-rule,data=X,family="binomial") #Model 2: drops the predictors that were not significant in model 1 (education, Black county government, BLM protests)
summary(model2)

#note: in models 3 and 4 the main effects of CRTINDEX (and in model 3 of rSLV) are left out,
#so the product terms are not full interactions and should be read as plain product variables
model3<-glm(active~.-id-rU-locBLC-BLM+rSLV*CRTINDEX-rSLV-CRTINDEX-rINC-rule,data=X,family="binomial") #Model 3: model 2 with slavery intensity x CRT index in place of the two separate variables
summary(model3)

model4<-glm(active~.-id-rU-locBLC-BLM+rB*CRTINDEX-rSLV-CRTINDEX-rINC-rule,data=X,family="binomial") #Model 4: model 2 with Black population share x CRT index in place of slavery intensity and the CRT index
summary(model4)

stargazer(model1,model2,model3,model4,title="Logistic regression models of monument removal",type="html",out="model_comparasion.html") #Side-by-side table of the four models, saved as HTML

# ---- 2. Fit statistics ----

PseudoR2(model1) #McFadden's pseudo R-squared of each model
PseudoR2(model2)
PseudoR2(model3)
PseudoR2(model4)

ROC<-roc(X$active,model3$fitted.values) #ROC curve of model 3 (the model with the lowest AIC)
AUC(ROC$specificities,ROC$sensitivities) #Area under the ROC curve
summary(model3) #Print model 3 again for reference
ggroc(ROC,size=3)+ggtitle("ROC curve, model 3")+xlab("Specificity")+ylab("Sensitivity")+geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed",size=3)+theme(plot.title = element_text(size = 40,hjust=0.5,family="serif"))+theme(axis.text=element_text(size=30,family="serif"),axis.title=element_text(size=30,family="serif")) #Plot the ROC curve against the diagonal of a random classifier

# ---- 3. Coefficient plot ----

df<-model3$coefficients #Coefficients of model 3
df<-data.frame(var=names(df),est=as.numeric(df)) #Put them into a data frame with the variable names
df$low<-df$est-2*coeftest(model3)[,2] #Lower end of the interval: estimate minus two standard errors
df$high<-df$est+2*coeftest(model3)[,2] #Upper end of the interval: estimate plus two standard errors

ggplot(df, aes(x = est, y = var)) +
  geom_point(color = "darkblue", size = 5) +                                  #point estimates
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.2, color = "blue") + #approximate 95% intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +         #reference line at zero
  labs(
    title = "Logistic regression coefficients (model 3)",
    x = "Coefficient (approx. 95% confidence interval)",
    y = NULL
  ) + theme(plot.title = element_text(size = 35,hjust=0.5,family="serif"))+theme(axis.text=element_text(size=30,family="serif"),axis.title=element_text(size=30,face="bold",family="serif"))


# ---- 4. Projection of expected removal years ----
#projection: convert the fitted 8-year removal probability into a constant yearly hazard,
#then compute the year by which each monument's cumulative removal probability reaches 50%
p<-1-(1-model3$fitted.values)**(1/8) #Yearly removal probability implied by the fitted probability over the 8-year window

t<-log(0.5)/(log(1-p)) #Number of years until the probability of still standing falls to 50%

chart<-data.frame(t=2015+ceiling(t)) #Convert to calendar years, counting from 2015

chart<-data.frame(t=chart[chart$t<2101,]) #Keep projections within this century

ggplot(data=chart,aes(x=t))+geom_density(aes(y=after_stat(count)),fill="darkblue",adjust=1)+xlab("Year")+ylab("Expected number of removals")+ggtitle("Distribution of expected removal years")+ theme(plot.title = element_text(size = 35,hjust=0.5,family="serif"))+theme(axis.text=element_text(size=25,family="serif"),axis.title=element_text(size=25,face="bold",family="serif")) #Distribution of the projected removal years

# ---- 5. Classification accuracy ----
#classification at the threshold that maximises Youden's J
trs<-coords(ROC, x="best", best.method="youden") #Threshold with the best combined sensitivity and specificity
point<-data.frame(real=model3$model[,1]==1,guess=model3$fitted.values>trs$threshold) #Actual outcome vs predicted outcome at that threshold
table(point) #Confusion matrix
sum(diag(table(point)))/nrow(point) #accuracy
