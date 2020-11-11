
#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

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
## read data 
#######################################################
data_shoppingCovid19 <- read_rds("Data/raw_shoppingCovid19_clean.rds")
dim(data_shoppingCovid19)


#######################################################
## calculate scales
#######################################################

# COSS (online shopping)
data_shoppingCovid19$COSS <- rowSums(data_shoppingCovid19[, grep("coss_", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_CONFLICT <- rowSums(data_shoppingCovid19[, grep("coss_conflict", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_SALIENCE <- rowSums(data_shoppingCovid19[, grep("coss_salience", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_MOOD_MOD <- rowSums(data_shoppingCovid19[, grep("coss_moodMod", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_TOLERANCE <- rowSums(data_shoppingCovid19[, grep("coss_tolerance", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_RELAPSE <- rowSums(data_shoppingCovid19[, grep("coss_relapse", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_WITHDRAWAL <- rowSums(data_shoppingCovid19[, grep("coss_withdrawal", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$COSS_PROBLEMS <- rowSums(data_shoppingCovid19[, grep("coss_problems", colnames(data_shoppingCovid19), value=T)], na.rm=F)

# Bergen shopping addiction scale (offline shopping)
data_shoppingCovid19$BERGEN <- rowSums(data_shoppingCovid19[, grep("bergen_", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGENS_CONFLICT <- rowSums(data_shoppingCovid19[, grep("bergen_conflict", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_SALIENCE <- rowSums(data_shoppingCovid19[, grep("bergen_salience", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_MOOD_MOD <- rowSums(data_shoppingCovid19[, grep("bergen_moodMod", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_TOLERANCE <- rowSums(data_shoppingCovid19[, grep("bergen_tolerance", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_RELAPSE <- rowSums(data_shoppingCovid19[, grep("bergen_relapse", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_WITHDRAWAL <- rowSums(data_shoppingCovid19[, grep("bergen_withdrawal", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$BERGEN_PROBLEMS <- rowSums(data_shoppingCovid19[, grep("bergen_problems", colnames(data_shoppingCovid19), value=T)], na.rm=F)

# CISS to measure coping
data_shoppingCovid19$CISS <- rowSums(data_shoppingCovid19[, grep("ciss_", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$CISS_TASK <- rowSums(data_shoppingCovid19[, grep("ciss_.+.task", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$CISS_EMOTION <- rowSums(data_shoppingCovid19[, grep("ciss_.+.emot", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$CISS_TREAT <- rowSums(data_shoppingCovid19[, grep("ciss_.+.treat", colnames(data_shoppingCovid19), value=T)], na.rm=F)
data_shoppingCovid19$CISS_CONTACT <- rowSums(data_shoppingCovid19[, grep("ciss_.+.contact", colnames(data_shoppingCovid19), value=T)], na.rm=F)

# PSS to measure stress 
# first reverse items
data_shoppingCovid19$pss_4 <- 6 - data_shoppingCovid19$pss_4r
data_shoppingCovid19$pss_5 <- 6 - data_shoppingCovid19$pss_5r
data_shoppingCovid19$pss_6 <- 6 - data_shoppingCovid19$pss_6r
data_shoppingCovid19$pss_7 <- 6 - data_shoppingCovid19$pss_7r
data_shoppingCovid19$pss_9 <- 6 - data_shoppingCovid19$pss_9r
data_shoppingCovid19$pss_10 <- 6 - data_shoppingCovid19$pss_10r
data_shoppingCovid19$pss_13 <- 6 - data_shoppingCovid19$pss_13r

# calculate scale
data_shoppingCovid19$PSS <- rowSums(data_shoppingCovid19[, grep("pss_\\d+$", colnames(data_shoppingCovid19), value=T)], na.rm=F)

#######################################################
## save final data with scales
#######################################################
write_rds(data_shoppingCovid19, path="Data/data_shoppingCovid19_withScales.rds")


