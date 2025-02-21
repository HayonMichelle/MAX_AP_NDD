###################################################################################################################
## Sample R code for the MAX_AP_NDD project
## "Perinatal and early childhood exposure to greenspace and risk of neurodevelopmental delays: a national birth cohort study among Medicaid enrollees" 
## Hayon Michelle Choi
## February, 2025
###################################################################################################################

library(survival); 

# DEFINE DIRECTORY
dir <- "D:/MAX/MAX_AP_NDD/Analysis"

# Read dataset
data<-load(paste(dir,"MAX_AP_NDD.rds"))

####################################################################################
# Covariates
covariates<-c("Race_WHITE","Race_BLACK_OR_AFRICAN_AMERICAN","Race_HISPANIC_OR_LATINO",  
              "Race_ASIAN_OR_OTHER_PACIFIC","Age", "temp",
              "MH","nearest_hospital_km","medhouseholdincome",
             "Substance_abuse","Smoking","Alcohol_abuse","BMI","Poor_nutrition")

####################################################################################

# MODELS by NDD
formula1<-as.formula("Surv(FU_Event_1_Correct, Event_1_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula2<-as.formula("Surv(FU_Event_3_Correct, Event_3_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula3<-as.formula("Surv(FU_Event_5_Correct, Event_5_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula4<-as.formula("Surv(FU_Event_6_Correct, Event_6_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula5<-as.formula("Surv(FU_Event_7_Correct, Event_7_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula6<-as.formula("Surv(FU_Event_9_Correct, Event_9_MAIN) ~  ndvi_preg +as.factor(season_deliv) + strata(FIPS)")
formula7<-as.formula("Surv(FU_Event_12_Correct, Event_12_MAIN) ~ ndvi_preg +as.factor(season_deliv) + strata(FIPS)")


################################################################################
# RUN THE MODELS

model1<- coxph(update(formula1, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model2<- coxph(update(formula2, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model3<- coxph(update(formula3, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model4<- coxph(update(formula4, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model5<- coxph(update(formula5, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model6<- coxph(update(formula6, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)
model7<- coxph(update(formula7, reformulate(c(".",covariates))), 
                 cluster=ZIP_Index,data = data)

iqr_evi<-IQR(data$ndvi_preg, na.rm=T)

result<-matrix(NA, nrow=7, ncol=3)

result[1,1]<-exp(coef(model1)*iqr_evi)[1]
result[1,2]<-exp((coef(model1)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model1)[1,1])))
result[1,3]<-exp((coef(model1)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model1)[1,1])))

result[2,1]<-exp(coef(model2)*iqr_evi)[1]
result[2,2]<-exp((coef(model2)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model2)[1,1])))
result[2,3]<-exp((coef(model2)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model2)[1,1])))

result[3,1]<-exp(coef(model3)*iqr_evi)[1]
result[3,2]<-exp((coef(model3)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model3)[1,1])))
result[3,3]<-exp((coef(model3)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model3)[1,1])))

result[4,1]<-exp(coef(model4)*iqr_evi)[1]
result[4,2]<-exp((coef(model4)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model4)[1,1])))
result[4,3]<-exp((coef(model4)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model4)[1,1])))

result[5,1]<-exp(coef(model5)*iqr_evi)[1]
result[5,2]<-exp((coef(model5)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model5)[1,1])))
result[5,3]<-exp((coef(model5)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model5)[1,1])))

result[6,1]<-exp(coef(model6)*iqr_evi)[1]
result[6,2]<-exp((coef(model6)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model6)[1,1])))
result[6,3]<-exp((coef(model6)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model6)[1,1])))

result[7,1]<-exp(coef(model7)*iqr_evi)[1]
result[7,2]<-exp((coef(model7)[1]*iqr_evi-1.96*iqr_evi*sqrt(vcov(model7)[1,1])))
result[7,3]<-exp((coef(model7)[1]*iqr_evi+1.96*iqr_evi*sqrt(vcov(model7)[1,1])))

#result

##Save the results 
# save.image(paste(dir,"Output/Main_result.RData",sep="/"))
