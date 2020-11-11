
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
data_shoppingCovid19 <- read_rds("Data/data_shoppingCovid19_withScales.rds")
dim(data_shoppingCovid19)

#######################################################
## adjust factor levels
#######################################################
data_shoppingCovid19$gender <- as.factor(data_shoppingCovid19$gender)
levels(data_shoppingCovid19$gender) <- c("1" = "female", "2" = "male", "3" = "other", "4" = "does not want to tell")

# SES
as.factor(data_shoppingCovid19$SES_subj)
levels(data_shoppingCovid19$SES_subj) <- c("NA", levels(data_shoppingCovid19$SES_subj))
                                           
# income
# income <- c("income_before", "income_now")
# for (i in income) {
#   data_shoppingCovid19[, i] <- factor(data_shoppingCovid19[, i], 
#                                       levels = c("under_15", "15_25k", "25_35k", "35_50k", "50-75k", "75_100k", "above_100k", ""))
#   levels(data_shoppingCovid19[, i]) <- c("under_15", "15_25k", "25_35k", "35_50k", "50-75k", "75_100k", "above_100k", "does not want to tell")
# }

initial_levels <- c("under_15", "15_25k", "25_35k", "35_50k", "50-75k", "75_100k", "above_100k", "")
initial_labels <- c("under 15k", "15-25k", "25-35k", "35-50k", "50-75k", "75-100k", "above 100k",
                    "Does not want to tell")
data_shoppingCovid19$income_before <- factor(data_shoppingCovid19$income_before,
                                             levels=initial_levels,
                                             labels=initial_labels)

data_shoppingCovid19$income_now <- factor(data_shoppingCovid19$income_now,
                                          levels=initial_levels,
                                          labels=initial_labels)

# debts
amounts <- c("credit_card_amount", "debt_amount")
for (i in amounts) {
  data_shoppingCovid19[, i] <- factor(data_shoppingCovid19[, i], 
                                      levels = c("less_than_100", "100_250", "250_600", "600_1200", "4000_10000",  "over_10000", ""))
  # levels = c("less_than_100", "100_250", "250_600", "600_1200", "1200-2000", "2000-4000", "4000_10000",  "over_10000", ""))
  levels(data_shoppingCovid19[, i]) <- c("less_than_100", "100_250", "250_600", "600_1200", "4000_10000", "over_10000", "does not want to tell")
}

data_shoppingCovid19$SES_subj <- factor(data_shoppingCovid19$SES_subj, 
                                        levels = c("poorest", "poorer", "poor", "average", "rich", "richer", "richest"))

data_behav_addictions <-  c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug",
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq")

for (i in data_behav_addictions) {
  data_shoppingCovid19[, i] <- factor(data_shoppingCovid19[, i],
                                             labels = c("not at all", "a little bit", "somewhat", "quite a lot", "too much"))
}

# study
data_shoppingCovid19$study <- as.factor(data_shoppingCovid19$study)
levels(data_shoppingCovid19$study) <- c("1" = "yes, studies", "2" = "no, does not study")

# work
data_shoppingCovid19$work <- as.factor(data_shoppingCovid19$work)
levels(data_shoppingCovid19$work) <- c("1" = "full time", "2" = "part time", "3" = "less than part time", "4" = "no work")

#######################################################
## socio-demographic factors 
#######################################################
socio_demographics <- 
  list(
"N (all rows, all columns" = dim(data_shoppingCovid19),
"N no missing" = sum(!is.na(data_shoppingCovid19$age2)),
"Age" = table(data_shoppingCovid19$age), 
"Age mean" = mean(data_shoppingCovid19$age, na.rm=T), 
"Age SD" = sd(data_shoppingCovid19$age, na.rm = T), 
"Gender, where 1=female, 2=male, 3=other, 4=do not want to tell" = round(prop.table(table(data_shoppingCovid19$gender))*100, 2), 
"Education highest, where 0=elementary/noFormal, 1=primary, 2=secondary/high, 3=college, 4=undergrad, 5=grad, 6=phd/mba" = 
  table(data_shoppingCovid19$edu_highest),
"Education highest %, where 0=elementary/noFormal, 1=primary, 2=secondary/high, 3=college, 4=undergrad, 5=grad, 6=phd/mba" = 
  round(prop.table(table(data_shoppingCovid19$edu_highest))*100, 2),
"State" = table(data_shoppingCovid19$state), 
"Work status, where 1=full-time, 2=part-time, 3=less than PT, 4=no" = round(prop.table(table(data_shoppingCovid19$work))*100, 2), 
"Relationship status, where 1=in a relationship, 2=not" = round(prop.table(table(data_shoppingCovid19$relationship))*100, 2), 
"Relationship qual, where 1=married, 2=living together, 3=not living together, 4=dont want to tell" =round(prop.table(table(data_shoppingCovid19$relationship_qual))*100, 2),
"Income before Covid19 (monthly)" = round(prop.table(table(data_shoppingCovid19$income_before))*100, 2), 
"Income now (past 30 days)" = round(prop.table(table(data_shoppingCovid19$income_now))*100, 2), 
"SES subjective where 1=richest, 7=poorest" = round(prop.table(table(data_shoppingCovid19$edu_highest))*100, 2)
)

socio_demographics

#######################################################
## shopping-related factors
#######################################################
shopping_check <- list(
  "Made offline purchase (1=no, 2=yes)" = table(data_shoppingCovid19$offline_purchase), 
  "Made online purchase (1=no, 2=yes)" = table(data_shoppingCovid19$online_purchase), 
  "Credit card overdue (1=no, 2=yes)" = table(data_shoppingCovid19$credit_card), 
  "Credit card overdue amount (%)" = round(prop.table(table(data_shoppingCovid19$credit_card_amount))*100, 2), 
  "Owe company (1=no, 2=yes)" = table(data_shoppingCovid19$debt), 
  "Owe company, amount (%)" = round(prop.table(table(data_shoppingCovid19$debt_amount))*100, 2)
  )
shopping_check

#######################################################
## scales
#######################################################
# select scales to describe from the full data
scales_continous <- c("PSS", "SDS_honesty", "CISS", "BERGEN", "COSS")
data_scales_continous <- data_shoppingCovid19[, scales_continous]    

# create empty dataframe for the descriptives
descriptive_scales <- data.frame("Mean" = numeric(), "SD" = numeric(), "min" = numeric(), "max" = numeric(), "normality_SW" = numeric(), "normality_SW_p" = numeric())

# create descriptive statistics for the selected scales
for (i in 1:length(names(data_scales_continous))) {
  shapiroResult <- shapiro.test(data_scales_continous[, i])
  newRow <- list(mean(data_scales_continous[, i], na.rm=TRUE),
                 sd(data_scales_continous[, i], na.rm=TRUE),
                 min(data_scales_continous[, i], na.rm=TRUE),
                 max(data_scales_continous[, i], na.rm=TRUE),
                 shapiroResult[[1]], 
                 shapiroResult[[2]])
  descriptive_scales[names(data_scales_continous[i]), ] <-  newRow
}

# print descriptive dataframe
print(round(descriptive_scales, 3))


#######################################################
## save final data with scales and factor levels adjusted
#######################################################
write_rds(data_shoppingCovid19, path="Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
