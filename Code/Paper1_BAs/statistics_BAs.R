#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("plyr", "tidyverse", "reshape2", 
              "sjstats", "sjmisc", "psych")
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
## Correlation between the 2 stress measures
#######################################################
cor.test(data_shoppingCovid19$PSS, data_shoppingCovid19$stress_outbreak, method="kendall")




#######################################################
## H1: The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
#######################################################
# create a database, where data points are averaged for every 4 batch
data_shoppingCovid19$timeMerged <- group_var(data_shoppingCovid19$time_batch, size = 4, right.interval = T)

# prep data for corr. analysis
data_corr <- subset(data_shoppingCovid19, select = c("time_batch", BAs))
data_corr <- sapply(data_corr, as.numeric)

# calculate correlation for each BA
cor.test(~ time_batch + BAs_shopping, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_alcohol, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_smoking, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_legal_drug, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_illegal_drug, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_gambling, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_gaming, data=data_corr, method="kendall")
cor.test(~ time_batch + BAs_overeating, data=data_corr, method="kendall")



#######################################################
## H2: Self-reported addiction-related behavioral problems are related to distress (general + covid19 related) 
#######################################################

# transform BA variables into numeric (from factor)
data_shoppingCovid19[BAs] <- sapply(data_shoppingCovid19[BAs], as.numeric)

# calculate correlation STRESS OUTBREAK
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


# calculate correlation PSS
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

# test H2  (The frequency of self-reported addiction-related behavioral problems increases over time (as stress becomes chronic)
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_shopping, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_alcohol, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_smoking, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_legal_drug, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_illegal_drug, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_gambling, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_gaming, method="spearman")
cor.test(x=data_corr_PSS_BAs$time_days, y=data_corr_PSS_BAs$BAs_overeating, method="spearman")


# test H3 (Self-reported addiction-related behavioral problems are related to Covid19-related distress )
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_shopping, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_alcohol, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_smoking, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_legal_drug, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_illegal_drug, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_gambling, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_gaming, method="spearman")
cor.test(x=data_corr_stressOutbr_BAs$time_days, y=data_corr_stressOutbr_BAs$BAs_overeating, method="spearman")


