library(dplyr)
library(ggplot2)

seniors <- read.csv(file="C:\\Users\\Om\\OneDrive - Queen's University\\Winter 2018\\ECON 483\\Census_By_Community_2016_csv.csv", header=TRUE,sep = "\t", na.strings = "#N/A")

seniors$senior_chg_vs_pop <- seniors$Percent_Change_65OMF-((seniors$RES_CNT-seniors$X2014_RES_CNT)/seniors$X2014_RES_CNT)
seniors$stdizd_senior_chg<- seniors$Percent_Change_65OMF-0.0946517
seniors$stdized_chg_vs_pop <- seniors$stdizd_senior_chg -0.03626421

seniors$stdizd_senior_chg<- (seniors$Percent_Change_65OMF-mean(seniors$Percent_Change_65OMF))/sd(seniors$Percent_Change_65OMF)

seniors$stdizd_senior_chg<- (seniors$Actual_Change_65OMF-mean(seniors$Actual_Change_65OMF))/sd(seniors$Actual_Change_65OMF)

seniors$stdizd_senior_chg<- (seniors$Percent_Chg_65OvsTtl-mean(na.omit(seniors$Actual_Change_65OMF)))/sd(na.omit(seniors$Actual_Change_65OMF))

seniors$senior_prop_chg <- (seniors$Actual_Change_65OMF)/seniors$ttl_act_chg

seniors$ttl_chg <- (seniors$RES_CNT-seniors$X2014_RES_CNT)/seniors$X2014_RES_CNT
seniors$ttl_chg[is.infinite(seniors$ttl_chg)]<- NA
seniors$stdized_ttlchg <- (seniors$ttl_act_chg - mean(na.omit(seniors$ttl_act_chg)))/sd(na.omit(seniors$ttl_act_chg))

seniors$ttl_act_chg<- seniors$RES_CNT-seniors$X2014_RES_CNT
seniors$z <- ((seniors$Actual_Change_65OMF-seniors$ttl_act_chg)-(mean(seniors$Actual_Change_65OMF)-mean(na.omit(seniors$ttl_act_chg))))/(sqrt((sd(seniors$Actual_Change_65OMF)/length(seniors$Actual_Change_65OMF)+(sd(na.omit(seniors$ttl_act_chg))/length(na.omit(seniors$ttl_act_chg))))))
seniors$z<-scale(seniors$z)
seniors$stdized_chg_vs_pop <- seniors$stdizd_senior_chg - seniors$stdized_ttlchg

seniors$senior_chg_vs_pop[is.infinite(seniors$senior_chg_vs_pop)]<- NA
seniors_1<- seniors[complete.cases(seniors[,c("senior_chg_vs_pop")]),]
seniors_1$stdized_new <- (seniors_1$senior_chg_vs_pop-mean(seniors_1$senior_chg_vs_pop))/sd(seniors_1$senior_chg_vs_pop)

hist(seniors$Actual_Change_65OMF,breaks=25, main = "Change in Seniors per community 2014-16", xlab = "Change in # of Seniors", ylab = "Frequency of Communities")
  
cor(a,b)
a<-data$MF_65O
b<-data$X2014_MF65O
cor(seniors$Actual_Change_65OMF,seniors$Seniors_Services_Ttl)

##Senior Services
fit <- lm(seniors$Actual_Change_65OMF ~ seniors$Seniors....Abuse.Issues + seniors$Seniors...Death.and.Funerals + seniors$Seniors...Educational.Programs + seniors$Seniors...Financial.Assistance.and.Pensions + seniors$Seniors...Food.Assistance.and.Grocery.Shopping + seniors$Seniors...Government.Services + seniors$Seniors...Health.and.Safety + seniors$Seniors...Housing + seniors$Seniors...Legal.Information.and.Assistance + seniors$Seniors...Recreation + seniors$Seniors...Respite...Caregiver + seniors$Seniors...Transportation, data=seniors)
summary(fit) # show results

fit_65OvsTtl <- lm(seniors$Percent_Chg_65OvsTtl ~ seniors$Seniors....Abuse.Issues + seniors$Seniors...Death.and.Funerals + seniors$Seniors...Educational.Programs + seniors$Seniors...Financial.Assistance.and.Pensions + seniors$Seniors...Food.Assistance.and.Grocery.Shopping + seniors$Seniors...Government.Services + seniors$Seniors...Health.and.Safety + seniors$Seniors...Housing + seniors$Seniors...Legal.Information.and.Assistance + seniors$Seniors...Recreation + seniors$Seniors...Respite...Caregiver + seniors$Seniors...Transportation, data=seniors)
summary(fit_65OvsTtl)

fit_percent_chg <- lm(seniors$Percent_Change_65OMF ~ seniors$Seniors....Abuse.Issues + seniors$Seniors...Death.and.Funerals + seniors$Seniors...Educational.Programs + seniors$Seniors...Financial.Assistance.and.Pensions + seniors$Seniors...Food.Assistance.and.Grocery.Shopping + seniors$Seniors...Government.Services + seniors$Seniors...Health.and.Safety + seniors$Seniors...Housing + seniors$Seniors...Legal.Information.and.Assistance + seniors$Seniors...Recreation + seniors$Seniors...Respite...Caregiver + seniors$Seniors...Transportation, data=seniors)
summary(fit_percent_chg)

fit_percent_comm <- lm(seniors$MF_65O_PCT ~ seniors$Seniors....Abuse.Issues + seniors$Seniors...Death.and.Funerals + seniors$Seniors...Educational.Programs + seniors$Seniors...Financial.Assistance.and.Pensions + seniors$Seniors...Food.Assistance.and.Grocery.Shopping + seniors$Seniors...Government.Services + seniors$Seniors...Health.and.Safety + seniors$Seniors...Housing + seniors$Seniors...Legal.Information.and.Assistance + seniors$Seniors...Recreation + seniors$Seniors...Respite...Caregiver + seniors$Seniors...Transportation, data=seniors)
summary(fit_percent_comm)

fit_percent_stdized <- lm(seniors$stdizd_senior_chg ~ log1p(seniors$Seniors....Abuse.Issues) + log1p(seniors$Seniors...Death.and.Funerals) + log1p(seniors$Seniors...Educational.Programs) + log1p(seniors$Seniors...Financial.Assistance.and.Pensions) + log1p(seniors$Seniors...Food.Assistance.and.Grocery.Shopping) + log1p(seniors$Seniors...Government.Services) + log1p(seniors$Seniors...Health.and.Safety) + log1p(seniors$Seniors...Housing) + log1p(seniors$Seniors...Legal.Information.and.Assistance) + log1p(seniors$Seniors...Recreation) + log1p(seniors$Seniors...Respite...Caregiver) + log1p(seniors$Seniors...Transportation), data=seniors)
summary(fit_percent_stdized)

fit_percent_stdizedz <- lm(seniors$z ~ log1p(seniors$Seniors....Abuse.Issues) + log1p(seniors$Seniors...Death.and.Funerals) + log1p(seniors$Seniors...Educational.Programs) + log1p(seniors$Seniors...Financial.Assistance.and.Pensions) + log1p(seniors$Seniors...Food.Assistance.and.Grocery.Shopping) + log1p(seniors$Seniors...Government.Services) + log1p(seniors$Seniors...Health.and.Safety) + log1p(seniors$Seniors...Housing) + log1p(seniors$Seniors...Legal.Information.and.Assistance) + log1p(seniors$Seniors...Recreation) + log1p(seniors$Seniors...Respite...Caregiver) + log1p(seniors$Seniors...Transportation), data=seniors)
summary(fit_percent_stdizedz)


fit_percent_stdized_new <- lm(seniors_1$stdized_new ~ log1p(seniors_1$Seniors....Abuse.Issues) + log1p(seniors_1$Seniors...Death.and.Funerals) + log1p(seniors_1$Seniors...Educational.Programs) + log1p(seniors_1$Seniors...Financial.Assistance.and.Pensions) + log1p(seniors_1$Seniors...Food.Assistance.and.Grocery.Shopping) + log1p(seniors_1$Seniors...Government.Services) + log1p(seniors_1$Seniors...Health.and.Safety) + log1p(seniors_1$Seniors...Housing) + log1p(seniors_1$Seniors...Legal.Information.and.Assistance) + log1p(seniors_1$Seniors...Recreation) + log1p(seniors_1$Seniors...Respite...Caregiver) + log1p(seniors_1$Seniors...Transportation), data=seniors_1)
summary(fit_percent_stdized_new)

##Amenities
fit_services_actualchg <- lm(seniors$Percent_Change_65OMF ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_actualchg)

fit_services_65ottlchg <- lm(seniors$Percent_Chg_65OvsTtl ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_65ottlchg)

fit_services_pctg <- lm(seniors$MF_65O_PCT ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_pctg)

fit_services_pctchg <- lm(seniors$senior_chg_vs_pop ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_pctchg)

fit_services_stdized <- lm(seniors$stdized_chg_vs_pop ~ log1p(seniors$Attraction) + log1p(seniors$Hospital) + log1p(seniors$Library) + log1p(seniors$PHS_Clinic) + log1p(seniors$Community_Centre) + log1p(seniors$Social._Dev_Ctr), data=seniors)
summary(fit_services_stdized)

fit_services_stdized <- lm(seniors$stdizd_senior_chg ~ log1p(seniors$Attraction) + log1p(seniors$Hospital) + log1p(seniors$Library) + log1p(seniors$PHS_Clinic) + log1p(seniors$Community_Centre) + log1p(seniors$Social._Dev_Ctr), data=seniors)
summary(fit_services_stdized)


fit_services_stdized_ppl <- lm(seniors$stdized_ttlchg~ log1p(seniors$Attraction) + log1p(seniors$Hospital) + log1p(seniors$Library) + log1p(seniors$PHS_Clinic) + log1p(seniors$Community_Centre) + log1p(seniors$Social._Dev_Ctr), data=seniors)
summary(fit_services_stdized_ppl)

fit_services_stdized_pplz <- lm(seniors$z~ log1p(seniors$Attraction) + log1p(seniors$Hospital) + log1p(seniors$Library) + log1p(seniors$PHS_Clinic) + log1p(seniors$Community_Centre) + log1p(seniors$Social._Dev_Ctr), data=seniors)
summary(fit_services_stdized_pplz)

fit_services_pctchg <- lm(seniors$stdizd_senior_chg ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_pctchg)

fit_services_pctchg <- lm(seniors$stdizd_senior_chg ~ seniors$Attraction + seniors$Hospital + seniors$Library + seniors$PHS_Clinic + seniors$Community_Centre + seniors$Social._Dev_Ctr, data=seniors)
summary(fit_services_pctchg)
##crimes
seniors$Percent_Change_Crime <- (seniors$Crimes_2017-seniors$Crimes_2014)/seniors$Crimes_2014
crime_seniors<- na.omit(seniors)
seniors$Percent_Change_Crime[is.infinite(seniors$Percent_Change_Crime)]<- NA
seniors<- seniors[complete.cases(seniors[,c("Percent_Change_Crime","Crimes_2017")]),]

seniors$Crimes_2016<- seniors$Crimes_2017
seniors$Percent_Total_Crime_2016 <- seniors$Percent_Total_Crime_2017

fit_crime_stdized <- lm(crime_seniors$stdizd_senior_chg ~ log1p(crime_seniors$Crimes_2016) + (crime_seniors$Percent_Change_Crime)+ crime_seniors$Percent_Total_Crime_2016, data=crime_seniors, na.action = na.exclude)
summary(fit_crime_stdized)

fit_crime_stdizedz <- lm(crime_seniors$z ~ log1p(crime_seniors$Crimes_2016) + (crime_seniors$Percent_Change_Crime)+ crime_seniors$Percent_Total_Crime_2016, data=crime_seniors, na.action = na.exclude)
summary(fit_crime_stdizedz)

##LRT
fit_lrt_change <- lm(seniors$Percent_Change_65OMF ~ seniors$LRT_1km_radius+ seniors$LRT_2km_radius, data=seniors, na.action = na.exclude)
summary(fit_lrt_change)

fit_lrt_comm <- lm(seniors$MF_65O ~ seniors$LRT_1km_radius+ seniors$LRT_2km_radius, data=seniors, na.action = na.exclude)
summary(fit_lrt_comm)

fit_lrt_seniorsvspop <- lm(seniors$senior_chg_vs_pop ~ seniors$LRT_1km_radius+ seniors$LRT_2km_radius, data=seniors, na.action = na.exclude)
summary(fit_lrt_seniorsvspop)

fit_lrt_stdized <- lm(seniors$stdizd_senior_chg ~ log1p(seniors$LRT_1km_radius)+ log1p(seniors$LRT_2km_radius), data=seniors, na.action = na.exclude)
summary(fit_lrt_stdized)

fit_lrt_stdizedz <- lm(seniors$z ~ log1p(seniors$LRT_1km_radius)+ log1p(seniors$LRT_2km_radius), data=seniors, na.action = na.exclude)
summary(fit_lrt_stdizedz)

##Business Licenses
seniors$Business_Licenses_2016 <- seniors$Business_Licenses_2017
seniors$BL_new_2016_pct <- (seniors$Business_Licenses_2017-seniors$Business_Licenses)/seniors$Business_Licenses

fit_biz_stdized <- lm(seniors$stdizd_senior_chg~ log1p(seniors$EMPLYD_PCT)  +log1p(seniors$Business_Licenses)  + seniors$BL_new_2016_pct, data=seniors, na.action = na.exclude)
summary(fit_biz_stdized)

fit_biz_stdizedz <- lm(seniors$z~ +seniors$EMPLYD_PCT  + log1p(seniors$Business_Licenses)  + seniors$BL_new_2016_pct, data=seniors, na.action = na.exclude)
summary(fit_biz_stdizedz)

##Renting
seniors$Ownership_Chg <- (seniors$Homeowners_cnt -seniors$X2014_OWNSHP_CNT)
seniors$renting_act_chg <- (seniors$DWELL_CNT - seniors$Homeowners_cnt)-(seniors$X2014_DWELL_CNT - seniors$X2014_OWNSHP_CNT)
seniors$New_houses <- (seniors$DWELL_CNT - seniors$X2014_DWELL_CNT)
seniors$New_houses[is.infinite(seniors$New_houses)]<- NA
seniors$renting_act_chg[is.infinite(seniors$renting_act_chg)]<- NA
seniors$Ownership_Chg[is.infinite(seniors$Ownership_Chg)]<- NA
seniors$renting_act_chg[is.nan(seniors$renting_act_chg)]<- NA
seniors$Ownership_Chg[is.nan(seniors$Ownership_Chg)]<- NA
seniors$new_nursing_hm <- seniors$NURSING_HM - seniors$X2014_NURSING_HM

seniors$chg_dwell_1 <- seniors$DWELSZ_1 - seniors$X2014_DWELSZ_1
seniors$chg_dwell_2 <- seniors$DWELSZ_2 - seniors$X2014_DWELSZ_2
seniors$chg_dwell_3 <- seniors$DWELSZ_3 - seniors$X2014_DWELSZ_3
seniors$chg_dwell_4_5 <- seniors$DWELSZ_4_5 - seniors$X2014_DWELSZ_4_5
seniors$chg_dwell_6 <- seniors$DWELSZ_6 - seniors$X2014_DWELSZ_6

seniors$pct_chg_dwell_1 <- seniors$chg_dwell_1/seniors$X2014_DWELSZ_1
seniors$pct_chg_dwell_2 <- seniors$chg_dwell_2/seniors$X2014_DWELSZ_2
seniors$pct_chg_dwell_3 <- seniors$chg_dwell_3/seniors$X2014_DWELSZ_3
seniors$pct_chg_dwell_4_5 <- seniors$chg_dwell_4_5/seniors$X2014_DWELSZ_4_5
seniors$pct_chg_dwell_6 <- seniors$chg_dwell_6/seniors$X2014_DWELSZ_6

seniors$pct_chg_dwell_1[is.infinite(seniors$pct_chg_dwell_1)]<- NA
seniors$pct_chg_dwell_2[is.infinite(seniors$pct_chg_dwell_2)]<- NA
seniors$pct_chg_dwell_3[is.infinite(seniors$pct_chg_dwell_3)]<- NA
seniors$pct_chg_dwell_4_5[is.infinite(seniors$pct_chg_dwell_4_5)]<- NA
seniors$pct_chg_dwell_6[is.infinite(seniors$pct_chg_dwell_6)]<- NA



fit_own <- lm(seniors$stdizd_senior_chg~ (New_houses)+ seniors$Median.assessed.value  + seniors$Ownership_Chg   + seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6, data=seniors, na.action = na.exclude)
summary(fit_own)

fit_ownz <- lm(seniors$z~ (New_houses) + seniors$Median.assessed.value + (seniors$renting_act_chg) +(seniors$Ownership_Chg) + seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6, data=seniors, na.action = na.exclude)
summary(fit_ownz)

fit_ownz <- lm(seniors$z~ (New_houses) + seniors$Median.assessed.value + (seniors$renting_act_chg) + (seniors$Ownership_Chg) + seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6 + seniors$pct_chg_dwell_1 + seniors$pct_chg_dwell_2 + seniors$pct_chg_dwell_3 + seniors$pct_chg_dwell_4_5 + seniors$pct_chg_dwell_6, data=seniors, na.action = na.exclude)
summary(fit_ownz)


##median value
seniors$assessedvalpc
seniors$assessedvalpc <- (seniors$Median.assessed.value/1000)*(seniors$DWELL_CNT)/seniors$RES_CNT
seniors$assessedvalpc[is.infinite(seniors$assessedvalpc)]<- NA
seniors$median_assessed_val <- seniors$Median.assessed.value
fit_med <- lm(seniors$stdizd_senior_chg ~ (seniors$median_assessed_val) + (seniors$assessedvalpc), data=seniors, na.action = na.exclude)
summary(fit_med)

fit_med <- lm(seniors$stdizd_senior_chg ~ (seniors$Median.assessed.value), data=seniors, na.action = na.exclude)
summary(fit_med)

fit_medz <- lm(seniors$z~ (seniors$Median.assessed.value) , data=seniors, na.action = na.exclude)
summary(fit_medz)

##employment
seniors$chg_emplymt_pct <- seniors$EMPLYD_PCT - seniors$X2014_EMPLYD_CNT/seniors$X2014_RES_CNT
seniors$chg_emplymt_pct[is.infinite(seniors$chg_emplymt_pct)]<- NA

fit_emp_sen <- lm(seniors$stdizd_senior_chg ~ (seniors$Median.assessed.value) + log1p(seniors$EMPLYD_PCT), data=seniors, na.action = na.exclude)
summary(fit_emp_sen)

fit_emp <- lm(seniors$z ~ (seniors$Median.assessed.value) +log1p(seniors$EMPLYD_PCT) , data=seniors, na.action = na.exclude)
summary(fit_emp)

##FINAL
fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~ log1p(seniors$Seniors...Respite...Caregiver)+ seniors$Attraction + seniors$Hospital + seniors$Community_Centre + seniors$Crimes_2016 + seniors$Percent_Change_Crime + seniors$EMPLYD_PCT +seniors$BL_new_2016_pct +seniors$Median.assessed.value + seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6 + (seniors$New_houses) +seniors$renting_act_chg , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~ seniors$Seniors...Respite...Caregiver+ seniors$Attraction + seniors$Hospital + seniors$Community_Centre + seniors$Crimes_2016 + seniors$Percent_Change_Crime + seniors$EMPLYD_PCT +seniors$BL_new_2016_pct  + seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6 + (seniors$New_houses) +seniors$renting_act_chg , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

vif(fit_final_just_seniors)

fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~ seniors$Seniors...Respite...Caregiver+ seniors$Hospital  + seniors$Crimes_2016 + seniors$Percent_Change_Crime + seniors$EMPLYD_PCT +seniors$Median.assessed.value +seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~  +seniors$Seniors...Respite...Caregiver + seniors$Crimes_2016 + seniors$EMPLYD_PCT +seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~ seniors$Seniors...Respite...Caregiver +seniors$EMPLYD_PCT  +seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

fit_final_just_seniors <- lm(seniors$stdizd_senior_chg ~ seniors$Median.assessed.value +log1p(seniors$EMPLYD_PCT) +seniors$new_nursing_hm + seniors$chg_dwell_1 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniors)

fit_final_just_seniorsz <- lm(seniors$z ~ (seniors$Seniors...Respite...Caregiver)+ seniors$Attraction + seniors$Hospital + seniors$Community_Centre + seniors$Crimes_2016 + seniors$Percent_Change_Crime + seniors$EMPLYD_PCT +seniors$BL_new_2016_pct +seniors$Median.assessed.value + seniors$new_nursing_hm , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniorsz)

fit_final_just_seniorsz <- lm(seniors$z ~ (seniors$Seniors...Respite...Caregiver)+ seniors$Attraction + seniors$Hospital + seniors$Community_Centre +  seniors$Percent_Change_Crime + seniors$EMPLYD_PCT +seniors$BL_new_2016_pct +seniors$Median.assessed.value + seniors$new_nursing_hm , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniorsz)

fit_final_just_seniorsz <- lm(seniors$z ~  seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniorsz)

fit_final_just_seniorsz <- lm(seniors$z ~  seniors$chg_dwell_2 + seniors$chg_dwell_3 + seniors$chg_dwell_4_5 + seniors$chg_dwell_6 , data = seniors, na.action = na.exclude)
summary(fit_final_just_seniorsz)

vif(fit_final_just_seniorsz)

library(rJava)
library(xlsx)
write.table(seniors, "C:\\Users\\Om\\OneDrive - Queen's University\\Winter 2018\\ECON 483\\mydata.csv", sep="\t")

f <- as.formula(paste('stdizd_senior_chg ~', paste("log1p(",colnames(seniors)[160:207],")", collapse='+')))
modelAllHexSubscales <- lm(f, data= seniors, na.action = na.exclude)
summary(modelAllHexSubscales)

g <- as.formula(paste('stdizd_senior_chg ~', paste(colnames(seniors)[160:207], collapse='+')))
modelAllHexSubscalesg <- lm(g, data= seniors, na.action = na.exclude)
summary(modelAllHexSubscalesg)

h <- as.formula(paste('z ~', paste(colnames(seniors)[190:207], collapse='+')))
modelAllHexSubscalesh <- lm(h, data= seniors, na.action = na.exclude)
summary(modelAllHexSubscalesh)


newcomms <- seniors[seniors$New_houses>1000,]
sum(newcomms$MF_65O,na.rm= TRUE)
sum(newcomms$Actual_Change_65OMF,na.rm= TRUE)
sum(newcomms$RES_CNT,na.rm= TRUE)
sum(newcomms$RES_CNT-newcomms$X2014_RES_CNT
    ,na.rm= TRUE)
