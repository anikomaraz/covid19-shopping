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


#######################################################
## H3: Self-reported addiction-related behavioral problems are related to Covid19-related distress
#######################################################
# calculate a correlation coefficient (Kendall's rank correlation, but please calculate Spearman too for cross-checking) between each behavioural problem and the variable "stress_outbreak" - a one-item indicator
# (Eva)



