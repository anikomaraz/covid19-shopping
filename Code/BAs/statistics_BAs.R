#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", 
              "sjstats")
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

#######################################################
## H1: The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
#######################################################
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")

# test the linearity among correlation coefficients calculated at each point of time
# (Aniko)

#######################################################
## H2: Self-reported addiction-related behavioral problems are related to general distress
#######################################################
# list each addiction-related behavioural problems
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")

# calculate a correlation coefficient (Spearman) between each behavioural problem and PSS, the total score of the past-7-day distress scale
# see this guide for choosing a corr.coeff.: https://journals.sagepub.com/doi/pdf/10.1177/8756479308317006
#(Eva)
library(car)
#creat data.frame with relevant variables
dataframeH2<- data.frame(as.numeric(data_shoppingCovid19$BAs_shopping), as.numeric(data_shoppingCovid19$BAs_alcohol), 
                                    as.numeric(data_shoppingCovid19$BAs_smoking), as.numeric(data_shoppingCovid19$BAs_legal_drug),
                                    as.numeric(data_shoppingCovid19$BAs_illegal_drug), as.numeric(data_shoppingCovid19$BAs_gambling),
                                    as.numeric(data_shoppingCovid19$BAs_gaming), as.numeric(data_shoppingCovid19$BAs_overeating), data_shoppingCovid19$PSS)
#rename colums
colnames(dataframeH2)
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_shopping."] <- "BAs_shopping"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_alcohol."] <- "BAs_alcohol"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_smoking."] <- "BAs_smoking"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_legal_drug."] <- "BAs_legal_drug"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_illegal_drug."] <- "BAs_illegal_drug"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_gambling."] <- "BAs_gambling"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_gaming."] <- "BAs_gaming"
names(dataframeH2)[names(dataframeH2) == "as.numeric.data_shoppingCovid19.BAs_overeating."] <- "BAs_overeating"
names(dataframeH2)[names(dataframeH2) == "data_shoppingCovid19.PSS"] <- "PSS"

#Tranformation: categorial answers into numeric 
dataframeH2$BAs_shopping <- recode(var=dataframeH2$BAs_shopping,
                                    recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_alcohol <- recode(var=dataframeH2$BAs_alcohol,
                                   recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_smoking <- recode(var=dataframeH2$BAs_smoking,
                                   recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_legal_drug <- recode(var=dataframeH2$BAs_legal_drug,
                                  recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_illegal_drug <- recode(var=dataframeH2$BAs_illegal_drug,
                                   recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_gambling <- recode(var=dataframeH2$BAs_gambling,
                                  recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_gaming <- recode(var=dataframeH2$BAs_gaming,
                                  recodes="'not at all'= 1; 'a little bit'= 2; 
                                   'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")
dataframeH2$BAs_overeating <- recode(var=dataframeH2$BAs_overeating, 
                                     recodes = "'not at all'= 1; 'a little bit'= 2;
                                     'somewhat'= 3; 'quite a lot'= 4; 'too much' = 5")

#Correlations between each behavioural problem and PSS
cor.test(dataframeH2$BAs_shopping, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_alcohol, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_smoking, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_legal_drug, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_illegal_drug, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_gambling, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_gaming, dataframeH2$PSS, method=c("spearman"), na.rm = T)
cor.test(dataframeH2$BAs_overeating, dataframeH2$PSS, method=c("spearman"), na.rm = T)


#######################################################
## H3: Self-reported addiction-related behavioral problems are related to Covid19-related distress
#######################################################
# calculate a correlation coefficient (Kendall's rank correlation, but please calculate Spearman too for cross-checking) between each behavioural problem and the variable "stress_outbreak" - a one-item indicator
# (Eva)



