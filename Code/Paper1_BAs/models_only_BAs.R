#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "ddplyr",
              "sjstats", "sjmisc")
# write a function to load (and/or install) packages with feedback of success
load_my_packages <- function(package){
  new.package <- packages[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

# load my packages
load_my_packages(packages)


#######################################################
## read data 
#######################################################
data_shoppingCovid19 <- read_rds("Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
dim(data_shoppingCovid19)

# create label for BA items only
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")


#######################################################
## H1: The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
#######################################################
# create a database, where data points are averaged for every 4 batch
data_shoppingCovid19$timeMerged <- group_var(data_shoppingCovid19$time_batch, size = 4, right.interval = T)

# calculate mean per merged time group
# but first grep pss items
# pss_items <- names(data_shoppingCovid19[, grep("pss_\\d+$", colnames(data_shoppingCovid19))])
# # and now calculate the aggregated means
# data_shoppingCovid19_timeAveraged <- aggregate(. ~ data_shoppingCovid19$timeMerged, 
#                                                data_shoppingCovid19[, c(pss_items, BAs, 
#                                                                         "stress_outbreak")],
#                                                function(x) mean(x, na.rm=TRUE), na.action = na.pass)

#alcohol
#model specification 
modelalc <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_alcohol ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlyalc <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_alcohol ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitalc <- sem(model_obs_onlyalc, data=data_shoppingCovid19)
fitalc2 <- sem(modelalc, data=data_shoppingCovid19)
summary(fitalc, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitalc2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitalc, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitalc2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#smoking
#model specification 
modelsmoke <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_smoking ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlysmoke <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_smoking ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitsmoke <- sem(model_obs_onlysmoke, data=data_shoppingCovid19)
fitsmoke2 <- sem(modelsmoke, data=data_shoppingCovid19)
summary(fitsmoke, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitsmoke2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitsmoke, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitsmoke2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#gaming
#model specification 
modelgaming <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_gaming ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlygaming <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_gaming ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitgaming <- sem(model_obs_onlygaming, data=data_shoppingCovid19)
fitgaming2 <- sem(modelgaming, data=data_shoppingCovid19)
summary(fitgaming, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitgaming2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitsmoke, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitsmoke2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#gambling
#model specification 
modelgambling <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_gambling ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlygambling <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_gambling ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitgambling <- sem(model_obs_onlygambling, data=data_shoppingCovid19)
fitgambling2 <- sem(modelgambling, data=data_shoppingCovid19)
summary(fitgambling, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitgambling2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitgambling, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitgambling2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#legaldrugs
#model specification 
modellegadrugs <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_legal_drug ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlylegaldrugs <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_legal_drug ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitlegaldrugs <- sem(model_obs_onlylegaldrugs, data=data_shoppingCovid19)
fitlegaldrugs2 <- sem(modellegadrugs, data=data_shoppingCovid19)
summary(fitlegaldrugs, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitlegaldrugs2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitlegaldrugs, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitlegaldrugs2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#illegaldrugs
#model specification 
modelillegadrugs <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_illegal_drug ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlyillegaldrugs <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_illegal_drug ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitillegaldrugs <- sem(model_obs_onlyillegaldrugs, data=data_shoppingCovid19)
fitillegaldrugs2 <- sem(modelillegadrugs, data=data_shoppingCovid19)
summary(fitillegaldrugs, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitillegaldrugs2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitillegaldrugs, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitillegaldrugs2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 

#overeating 
#model specification 
modelovereating <- '
  # measurement model
    PSS_latent =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_7 + pss_8 + 
    pss_9 + pss_10 + pss_11 + pss_12 + pss_13 + pss_14
  # regressions
    PSS_latent ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_overeating ~ PSS_latent + stress_outbreak + timeMerged
  # correlations
    PSS_latent ~~ stress_outbreak
    '

model_obs_onlyovereating <- '
  # measurement model
  # regressions
    PSS ~ timeMerged
    stress_outbreak ~ timeMerged
    BAs_overeating ~ PSS + stress_outbreak + timeMerged
  # correlations
    PSS ~~ stress_outbreak
    '

#model identification ?
#t rule: 15 <= 0.5 * 14 * 15 --> True 

#model estimation 
#H0: SEM does not fit better to our data then the saturated model
#H1: SEM does fit worser to our data then the saturated model 
fitovereating <- sem(model_obs_onlyovereating, data=data_shoppingCovid19)
fitillegalovereating2 <- sem(modelovereating, data=data_shoppingCovid19)
summary(fitovereating, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures
summary(fitillegalovereating2, standardized=TRUE) #H0 is declined but we have a big sample so now have a look at the fit measures

fitMeasures(fitovereating, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 
fitMeasures(fitillegalovereating2, c("cfi","rmsea","rmsea.ci.lower", "rmsea.ci.upper")) # we do not have a good fit here 


#######################################################
## H2: Self-reported addiction-related behavioral problems are related to general distress
#######################################################

#######################################################
## H3: Self-reported addiction-related behavioral problems are related to Covid19-related distress
#######################################################

# transform BA variables into numeric (from factor)
data_shoppingCovid19[BAs] <- sapply(data_shoppingCovid19[BAs], as.numeric)

# calculate correlation
data_corr_stressOutbr_BAs <- 
  plyr::join_all(list(
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_shopping" = cor(as.numeric(BAs_shopping), stress_outbreak,
                               method = "spearman")),
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_alcohol" = cor(as.numeric(BAs_alcohol), stress_outbreak,
                              method = "spearman")),
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_smoking" = cor(as.numeric(BAs_smoking), stress_outbreak,
                              method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_legal_drug" = cor(as.numeric(BAs_legal_drug), stress_outbreak,
                                 method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_illegal_drug" = cor(as.numeric(BAs_illegal_drug), stress_outbreak,
                                   method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_gambling" = cor(as.numeric(BAs_gambling), stress_outbreak,
                               method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_gaming" = cor(as.numeric(BAs_gaming), stress_outbreak,
                             method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_overeating" = cor(as.numeric(BAs_overeating), stress_outbreak,
                                 method = "spearman"))
    
  )
  , by="time_days", type="full")


# join data by keeping the type of stress 
data_corr_stressType <- bind_rows(list(data_corr_stressOutbr_BAs, data_corr_PSS_BAs), .id="stress_type")
data_corr_stressType$stress_type <- as.factor(data_corr_stressType$stress_type)
data_corr_stressType$stress_type <- recode(data_corr_stressType$stress_type, "1" = "stress_outbreak", "2" = "PSS")

# test H1  and H2 (The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
corr.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_shopping, method="spearman")
corr.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_shopping, method="spearman")


# list each addiction-related behavioural problems
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")

# calculate a correlation coefficient (Spearman) between each behavioural problem and PSS, the total score of the past-7-day distress scale
# see this guide for choosing a corr.coeff.: https://journals.sagepub.com/doi/pdf/10.1177/8756479308317006

# correlation with PSS
data_corr_PSS_BAs <- 
  plyr::join_all(list(
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_shopping" = cor(as.numeric(BAs_shopping), PSS,
                               method = "spearman")),
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_alcohol" = cor(as.numeric(BAs_alcohol), PSS,
                              method = "spearman")),
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_smoking" = cor(as.numeric(BAs_smoking), PSS,
                              method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_legal_drug" = cor(as.numeric(BAs_legal_drug), PSS,
                                 method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_illegal_drug" = cor(as.numeric(BAs_illegal_drug), PSS,
                                   method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_gambling" = cor(as.numeric(BAs_gambling), PSS,
                               method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_gaming" = cor(as.numeric(BAs_gaming), PSS,
                             method = "spearman")), 
    ddply(data_shoppingCovid19, .(time_days), 
          summarise, 
          "BAs_overeating" = cor(as.numeric(BAs_overeating), PSS,
                                 method = "spearman"))
    
  )
  , by="time_days", type="full")


# join data by keeping the type of stress 
data_corr_stressType <- bind_rows(list(data_corr_stressOutbr_BAs, data_corr_PSS_BAs), .id="stress_type")
data_corr_stressType$stress_type <- as.factor(data_corr_stressType$stress_type)
data_corr_stressType$stress_type <- recode(data_corr_stressType$stress_type, "1" = "stress_outbreak", "2" = "PSS")

# test H1 (The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
corr.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_shopping, method="spearman")
corr.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_shopping, method="spearman")




################################## Eva's code to test H1###############################


# calculate a correlation coefficient (Kendall's rank correlation, but please calculate Spearman too for cross-checking) between each behavioural problem and the variable "stress_outbreak" - a one-item indicator
# (Eva)

dataframeH3 <- data.frame(dataframeH2[, 1:8], data_shoppingCovid19$stress_outbreak)
colnames(dataframeH3)
names(dataframeH3)[names(dataframeH3) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"

#Kendalls' rank correlation
cor.test(dataframeH3$BAs_shopping, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_alcohol, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_smoking, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_legal_drug, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_illegal_drug, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_gambling, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_gaming, dataframeH3$stress_outbreak, method="kendall", na.rm = T)
cor.test(dataframeH3$BAs_overeating, dataframeH3$stress_outbreak, method="kendall", na.rm = T)

#Spearman Correlation
cor.test(dataframeH3$BAs_shopping, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_alcohol, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_smoking, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_legal_drug, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_illegal_drug, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_gambling, dataframeH3$stress_outbreak, method="spearman", na.rm = T)
cor.test(dataframeH3$BAs_overeating, dataframeH3$stress_outbreak, method="spearman", na.rm = T)

#Merge 4 Ddatapoints into one new varible
#create new dataframe: all BAs, PSS, time batch and stress outbreak 
plotcorrdisandBA<- data.frame(as.numeric(data_shoppingCovid19$BAs_shopping), as.numeric(data_shoppingCovid19$BAs_alcohol), 
                              as.numeric(data_shoppingCovid19$BAs_smoking), as.numeric(data_shoppingCovid19$BAs_legal_drug),
                              as.numeric(data_shoppingCovid19$BAs_illegal_drug), as.numeric(data_shoppingCovid19$BAs_gambling),
                              as.numeric(data_shoppingCovid19$BAs_gaming), as.numeric(data_shoppingCovid19$BAs_overeating), 
                              as.numeric(data_shoppingCovid19$time_batch), data_shoppingCovid19$PSS, data_shoppingCovid19$pss_1, 
                              data_shoppingCovid19$pss_2, data_shoppingCovid19$pss_3, data_shoppingCovid19$pss_4, 
                              data_shoppingCovid19$pss_5, data_shoppingCovid19$pss_6, data_shoppingCovid19$pss_7, 
                              data_shoppingCovid19$pss_8, data_shoppingCovid19$pss_9, data_shoppingCovid19$pss_10, 
                              data_shoppingCovid19$pss_11, data_shoppingCovid19$pss_12, data_shoppingCovid19$pss_13, 
                              data_shoppingCovid19$pss_14 ,data_shoppingCovid19$stress_outbreak)

#rename colums
colnames(plotcorrdisandBA)
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_shopping."] <- "BAs_shopping"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_alcohol."] <- "BAs_alcohol"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_smoking."] <- "BAs_smoking"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_legal_drug."] <- "BAs_legal_drug"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_illegal_drug."] <- "BAs_illegal_drug"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_gambling."] <- "BAs_gambling"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_gaming."] <- "BAs_gaming"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_overeating."] <- "BAs_overeating"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.time_batch."] <- "time_batch"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.PSS"] <- "PSS"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_1"] <- "pss_1"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_2"] <- "pss_2"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_3"] <- "pss_3"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_4"] <- "pss_4"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_5"] <- "pss_5"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_6"] <- "pss_6"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_7"] <- "pss_7"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_8"] <- "pss_8"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_9"] <- "pss_9"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_10"] <- "pss_10"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_11"] <- "pss_11"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_12"] <- "pss_12"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_13"] <- "pss_13"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.pss_14"] <- "pss_14"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"
head(plotcorrdisandBA)

#average for every datapoint
library(plyr)
averageddatapoints <- ddply(plotcorrdisandBA, .(time_batch), summarize,  
                            Shopping=mean(BAs_shopping), Alcohol=mean(BAs_alcohol),
                            Smoking=mean(BAs_smoking), Legaldrugs=mean(BAs_legal_drug),
                            Illegaldrugs=mean(BAs_illegal_drug), Gaming=mean(BAs_gaming),
                            Gambling=mean(BAs_gambling), Overeating=mean(BAs_overeating),
                            PSS=mean(PSS), pss_1=mean(pss_1), pss_2=mean(pss_2), 
                            pss_3=mean(pss_3), pss_4=mean(pss_4), pss_5=mean(pss_5), 
                            pss_6=mean(pss_5), pss_6=mean(pss_6), pss_7=mean(pss_7), 
                            pss_8=mean(pss_8), pss_9=mean(pss_9), pss_10=mean(pss_10), 
                            pss_11=mean(pss_11), pss_12=mean(pss_12),pss_13=mean(pss_13),  
                            pss_14=mean(pss_14), stress_outbreak=mean(stress_outbreak))

library(groupdata2)
library(knitr) 
averageddatapoints = averageddatapoints %>%
  
  # Group data
  group(n = 4, method = 'greedy') %>%
  
  # Find means of each group
  dplyr::summarise(Shopping = mean(Shopping), Alkohol = mean(Alcohol), 
                   Smoking = mean(Smoking), Legaldrugs = mean(Legaldrugs), 
                   Illegaldrugs = mean(Illegaldrugs), Gaming = mean(Gaming),
                   Gambling = mean(Gambling), Overeating = mean(Overeating),
                   PSS = mean(PSS),  pss_1=mean(pss_1), pss_2=mean(pss_2), 
                   pss_3=mean(pss_3), pss_4=mean(pss_4), pss_5=mean(pss_5), 
                   pss_6=mean(pss_5), pss_6=mean(pss_6), pss_7=mean(pss_7), 
                   pss_8=mean(pss_8), pss_9=mean(pss_9), pss_10=mean(pss_10), 
                   pss_11=mean(pss_11), pss_12=mean(pss_12),pss_13=mean(pss_13),  
                   pss_14=mean(pss_14), stress_outbreak = mean(stress_outbreak), rm.na = T)

# Show new data
averageddatapoints %>% kable() 

colnames(averageddatapoints)
names(averageddatapoints)[names(averageddatapoints) == ".groups"] <- "time_merged"






