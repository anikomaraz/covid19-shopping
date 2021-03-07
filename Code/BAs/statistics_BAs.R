#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "ddplyr",
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
# transform BA variables into numeric (from factor)
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")

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
            
# test the linearity among correlation coefficients calculated at each point of time
# (Eva)

#make a nice new data.frame with all relevant variables for a great overview 
plotcorrdisandBA<- data.frame(as.numeric(data_shoppingCovid19$BAs_shopping), as.numeric(data_shoppingCovid19$BAs_alcohol), 
                              as.numeric(data_shoppingCovid19$BAs_smoking), as.numeric(data_shoppingCovid19$BAs_legal_drug),
                              as.numeric(data_shoppingCovid19$BAs_illegal_drug), as.numeric(data_shoppingCovid19$BAs_gambling),
                              as.numeric(data_shoppingCovid19$BAs_gaming), as.numeric(data_shoppingCovid19$BAs_overeating), 
                              as.numeric(data_shoppingCovid19$time_batch), data_shoppingCovid19$PSS, data_shoppingCovid19$stress_outbreak)

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
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"
head(plotcorrdisandBA)

#calculate correlation between BAs and PSS 
#shopping and PSS
require(plyr)
funcshopping <- function(plotcorrdisandBA)
{
  return(data.frame(CORshopping = cor(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$PSS )))
}
ashopping <-ddply(plotcorrdisandBA, .(time_batch), funcshopping)

#alcohol and PSS
require(plyr)
funcalcohol <- function(plotcorrdisandBA)
{
  return(data.frame(CORalcohol = cor(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$PSS )))
}
aalcohol <-ddply(plotcorrdisandBA, .(time_batch), funcalcohol)

#smoking and PSS
require(plyr)
funcsmoking <- function(plotcorrdisandBA)
{
  return(data.frame(CORsmoking = cor(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$PSS )))
}
asmoking <-ddply(plotcorrdisandBA, .(time_batch), funcsmoking)

#legal drug and PSS
require(plyr)
funclegaldrug <- function(plotcorrdisandBA)
{
  return(data.frame(CORlegaldrug = cor(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$PSS )))
}
alegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funclegaldrug)

#illegal drug and PSS
require(plyr)
funcillegaldrug <- function(plotcorrdisandBA)
{
  return(data.frame(CORillegaldrug = cor(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$PSS )))
}
aillegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funcillegaldrug)

#gambling and PSS
require(plyr)
funcgambling <- function(plotcorrdisandBA)
{
  return(data.frame(CORgambling = cor(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$PSS )))
}
agambling <-ddply(plotcorrdisandBA, .(time_batch), funcgambling)

#gaming and PSS
require(plyr)
funcgaming <- function(plotcorrdisandBA)
{
  return(data.frame(CORgaming = cor(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$PSS )))
}
agaming <-ddply(plotcorrdisandBA, .(time_batch), funcgaming)

#overeating and PSS
require(plyr)
funcovereating <- function(plotcorrdisandBA)
{
  return(data.frame(CORovereating = cor(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$PSS )))
}
aovereating <-ddply(plotcorrdisandBA, .(time_batch), funcovereating)

#make a dataframe for H1: Correlation BAs and PSS
H1BAsPSS <- data.frame(ashopping, aalcohol$CORalcohol, asmoking$CORsmoking, alegaldrug$CORlegaldrug, aillegaldrug$CORillegaldrug , agambling$CORgambling, agaming$CORgaming, aovereating$CORovereating)
colnames(H1BAsPSS) #tidying the colnames
names(H1BAsPSS)[names(H1BAsPSS) == "aalcohol.CORalcohol"] <- "CORalcohol"
names(H1BAsPSS)[names(H1BAsPSS) == "asmoking.CORsmoking"] <- "CORsmoking"
names(H1BAsPSS)[names(H1BAsPSS) == "alegaldrug.CORlegaldrug"] <- "CORlegaldrug"
names(H1BAsPSS)[names(H1BAsPSS) == "aillegaldrug.CORillegaldrug"] <- "CORillegaldrug"
names(H1BAsPSS)[names(H1BAsPSS) == "agambling.CORgambling"] <- "CORgambling"
names(H1BAsPSS)[names(H1BAsPSS) == "agaming.CORgaming"] <- "CORgaming"
names(H1BAsPSS)[names(H1BAsPSS) == "aovereating.CORovereating"] <- "CORovereating"


#calculate correlation between BAs and stress_outbreak
#shopping and stress_outbreak
require(plyr)
funcshoppingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORshopping = cor(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$stress_outbreak)))
}
bshopping <-ddply(plotcorrdisandBA, .(time_batch), funcshoppingb)

#alcohol and stress_outbreak
require(plyr)
funcalcoholb <- function(plotcorrdisandBA)
{
  return(data.frame(CORalcohol = cor(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$stress_outbreak)))
}
balcohol <-ddply(plotcorrdisandBA, .(time_batch), funcalcoholb)

#smoking and stress_outbreak
require(plyr)
funcsmokingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORsmoking = cor(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$stress_outbreak)))
}
bsmoking <-ddply(plotcorrdisandBA, .(time_batch), funcsmokingb)

#legal drug and stress_outbreak
require(plyr)
funclegaldrugb <- function(plotcorrdisandBA)
{
  return(data.frame(CORlegaldrug = cor(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$stress_outbreak)))
}
blegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funclegaldrugb)

#illegal drug and stress_outbreak
require(plyr)
funcillegaldrugb <- function(plotcorrdisandBA)
{
  return(data.frame(CORillegaldrug = cor(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$stress_outbreak)))
}
billegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funcillegaldrugb)

#gambling and stress_outbreak
require(plyr)
funcgamblingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORgambling = cor(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$stress_outbreak)))
}
bgambling <-ddply(plotcorrdisandBA, .(time_batch), funcgamblingb)

#gaming and stress_outbreak
require(plyr)
funcgamingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORgaming = cor(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$stress_outbreak)))
}
bgaming <-ddply(plotcorrdisandBA, .(time_batch), funcgamingb)

#overeating and stress_outbreak
require(plyr)
funcovereatingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORovereating = cor(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$stress_outbreak)))
}
bovereating <-ddply(plotcorrdisandBA, .(time_batch), funcovereatingb)

#make a dataframe for H1: Correlation BAs and stress_outbreak
H1BAsstressoutbreak <- data.frame(bshopping, balcohol$CORalcohol, bsmoking$CORsmoking, blegaldrug$CORlegaldrug, billegaldrug$CORillegaldrug , bgambling$CORgambling, bgaming$CORgaming, bovereating$CORovereating)
colnames(H1BAsstressoutbreak) #tidying the colnames
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "balcohol.CORalcohol"] <- "CORalcohol"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bsmoking.CORsmoking"] <- "CORsmoking"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "blegaldrug.CORlegaldrug"] <- "CORlegaldrug"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "billegaldrug.CORillegaldrug"] <- "CORillegaldrug"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bgambling.CORgambling"] <- "CORgambling"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bgaming.CORgaming"] <- "CORgaming"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bovereating.CORovereating"] <- "CORovereating"


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
cor.test(dataframeH2$BAs_shopping, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_alcohol, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_smoking, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_legal_drug, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_illegal_drug, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_gambling, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_gaming, dataframeH2$PSS, method="spearman", na.rm = T)
cor.test(dataframeH2$BAs_overeating, dataframeH2$PSS, method="spearman", na.rm = T) 


#######################################################
## H3: Self-reported addiction-related behavioral problems are related to Covid19-related distress
#######################################################
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
                              as.numeric(data_shoppingCovid19$time_batch), data_shoppingCovid19$PSS, data_shoppingCovid19$stress_outbreak)

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
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"
head(plotcorrdisandBA)

#average for every datapoint
library(plyr)
averageddatapoints <- ddply(plotcorrdisandBA, .(time_batch), summarize,  
                            Shopping=mean(BAs_shopping), Alcohol=mean(BAs_alcohol),
                            Smoking=mean(BAs_smoking), Legaldrugs=mean(BAs_legal_drug),
                            Illegaldrugs=mean(BAs_illegal_drug), Gaming=mean(BAs_gaming),
                            Gambling=mean(BAs_gambling), Overeating=mean(BAs_overeating),
                            PSS=mean(PSS), stress_outbreak=mean(stress_outbreak))

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
                   PSS = mean(PSS), stress_outbreak = mean(stress_outbreak), rm.na = T)

# Show new data
averageddatapoints %>% kable() 

