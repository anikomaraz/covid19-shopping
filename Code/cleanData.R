#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
install.packages("checkpoint")
library(checkpoint)
checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse")

# write a function to load packages with feedback of success
load_my_packages <- function(package){
  new.package <- packages[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

# load my packages
load_my_packages(packages)


#######################################################
## raw data and basic summary
#######################################################

# read raw data
raw_shoppingCovid19 <- read.csv2("./Data/shop_covid19_rawData_200415.csv")

# basic summary of raw data


