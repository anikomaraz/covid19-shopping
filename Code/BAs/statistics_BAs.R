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
data_shoppingCovid19 <- read_rds(path="../../Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
dim(data_shoppingCovid19)

#######################################################
## H1 
#######################################################
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking",
         "BAs_legal_drug", "BAs_illegal_drug", "BAs_gambling", 
         "BAs_gaming", "BAs_overeating")

library(nlme)
for (i in BAs) {
  # turn scales into numeric
  
  # get rid of missing data
  data <- subset(data_shoppingCovid19, !is.na(data_shoppingCovid19$BAs_shopping_num))
  
  # calculate fit
  lmeFit <- lme(BAs_shopping_num ~ time_batch, random=~1 | session,
              method="ML", 
              data=data_shoppingCovid19)
  anova(lmeFit)
  summary(lmeFit)
  
}

library(sjstats)
data_shoppingCovid19$id <- as.(1:length(data_shoppingCovid19$session))
data_shoppingCovid19$time_batch_xx <- as.numeric(data_shoppingCovid19$time_batch)

fit <- lme4::lmer(BAs_shopping_num ~ time_batch + (1 | time_batch), 
            data = data_shoppingCovid19)
summary(fit)

data_shoppingCovid19$time_batch_num <- as.numeric(data_shoppingCovid19$time_batch)
cor.test(data_shoppingCovid19$BAs_shopping_num, data_shoppingCovid19$time_batch_num, 
    method = "spearman")

