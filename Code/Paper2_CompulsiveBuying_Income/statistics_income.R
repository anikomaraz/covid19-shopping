#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# install.packages("checkpoint")
# library(checkpoint)
# checkpoint("2021-02-11")

## set up packages
packages <- c("tidyverse", "reshape2", 
              "effectsize", 
              "mediation", "gvlma", 
              "NSM3")

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

# restrict to those who have made an online purchase in the past 7 days
# data_shoppingCovid19_online <- subset(data_shoppingCovid19, online_purchase == "2") # meaning "yes"


#######################################################
## descriptive
#######################################################
# set data
data = data_shoppingCovid19

# socio-demographic characteristics
socio_demographics <- 
  list(
    "N (all rows, all columns" = dim(data),
    "N no missing" = sum(!is.na(data$age2)),
    "Age" = table(data$age), 
    "Age mean" = mean(data$age, na.rm=T), 
    "Age SD" = sd(data$age, na.rm = T), 
    "Gender, where 1=female, 2=male, 3=other, 4=do not want to tell" = round(prop.table(table(data$gender))*100, 2), 
    "Education highest, where 0=elementary/noFormal, 1=primary, 2=secondary/high, 3=college, 4=undergrad, 5=grad, 6=phd/mba" = 
      table(data$edu_highest),
    "Education highest %, where 0=elementary/noFormal, 1=primary, 2=secondary/high, 3=college, 4=undergrad, 5=grad, 6=phd/mba" = 
      round(prop.table(table(data$edu_highest))*100, 2),
    "State" = table(data$state), 
    "Work status, where 1=full-time, 2=part-time, 3=less than PT, 4=no" = round(prop.table(table(data$work))*100, 2), 
    "Relationship status, where 1=in a relationship, 2=not" = round(prop.table(table(data$relationship))*100, 2), 
    "Relationship qual, where 1=married, 2=living together, 3=not living together, 4=dont want to tell" =round(prop.table(table(data$relationship_qual))*100, 2),
    "Income before Covid19 (monthly)" = round(prop.table(table(data$income_before))*100, 2), 
    "Income now (past 30 days)" = round(prop.table(table(data$income_now))*100, 2), 
    "SES subjective where 1=richest, 7=poorest" = round(prop.table(table(data$edu_highest))*100, 2)
  )

socio_demographics

# shopping-related
shopping_check <- list(
  "Made offline purchase (1=no, 2=yes)" = table(data$offline_purchase), 
  "Made online purchase (1=no, 2=yes)" = table(data$online_purchase), 
  "Credit card overdue (1=no, 2=yes)" = table(data$credit_card), 
  "Credit card overdue amount (%)" = round(prop.table(table(data$credit_card_amount))*100, 2), 
  "Owe company (1=no, 2=yes)" = table(data$debt), 
  "Owe company, amount (%)" = round(prop.table(table(data$debt_amount))*100, 2)
)
shopping_check


#######################################################
## variable transformations
#######################################################
# merge groups of various "poor" and "rich" categories to increase power
data_shoppingCovid19$SES_3cat <- ifelse(data_shoppingCovid19$SES_subj %in% c("rich", "richer", "richest"), "RICH", 
                                        ifelse(data_shoppingCovid19$SES_subj %in% c("poor", "poorer", "poorest"), "POOR", 
                                               ifelse(data_shoppingCovid19$SES_subj == "average", "AVERAGE", 
                                                      NA)))


#######################################################
## AIM 1 Compulsive Buying increased in the first 6 months of Covid? 
#######################################################
## association between time and shopping
# online compulsive buying
cor.test(x=data_shoppingCovid19$time_days, y=data_shoppingCovid19$COSS, method = "kendall")
# offline compulsive buying
cor.test(x=data_shoppingCovid19$time_days, y=data_shoppingCovid19$BERGEN, method = "kendall")

# online CB according to SES groups
SES_3groups <- c("RICH", "AVERAGE", "POOR")
for (i in SES_3groups) {
data_ses_gr <- subset(data_shoppingCovid19, data_shoppingCovid19$SES_3cat == i)
    r <- cor.test(data_ses_gr$time_days, data_ses_gr$COSS, method = "kendall")
    print(r)
  }

# offline CB within SES groups
SES_3groups <- c("RICH", "AVERAGE", "POOR")
for (i in SES_3groups) {
  data_ses_gr <- subset(data_shoppingCovid19, data_shoppingCovid19$SES_3cat == i)
  r <- cor.test(data_ses_gr$time_days, data_ses_gr$BERGEN, method = "kendall")
  print(r)
}

# online CB according to income groups
income_3groups <- c("low", "medium", "high", "Does not want to tell")
for (i in income_3groups) {
  data_income_gr <- subset(data_shoppingCovid19, data_shoppingCovid19$income_3cat == i)
  r <- cor.test(data_income_gr$time_days, data_income_gr$COSS, method = "kendall")
  print(i)
  print(r)
}

# offline CB within income groups
for (i in income_3groups) {
  data_income_gr <- subset(data_shoppingCovid19, data_shoppingCovid19$income_3cat == i)
  r <- cor.test(data_income_gr$time_days, data_income_gr$BERGEN, method = "kendall")
  print(i)
  print(r)
}

#######################################################
## AIM 2 the effect of the stimulus package
#######################################################

# create variable to code pre-stimulus package data waves as "pre", and 2-weeks post-stimulus data collection as "post", all other as missing
data_shoppingCovid19$stimulus_package <- ifelse(data_shoppingCovid19$time_days <= 30, "pre", 
                                                ifelse(data_shoppingCovid19$time_days > 30 & data_shoppingCovid19$time_days < 45, "post",
                                                       NA))
# check result
table(data_shoppingCovid19$stimulus_package)
table(data_shoppingCovid19$SES_subj, data_shoppingCovid19$stimulus_package)



#######################################################
## Aim 3: Compulsive buying in response to distress during Covid
#######################################################

# SES group differences on ONLINE compulsive buying
summary(aov(COSS ~ SES_3cat, data = data_shoppingCovid19))
TukeyHSD(aov(COSS ~ SES_3cat, data = data_shoppingCovid19))

# calculate mean and sd per group
aggregate(data_shoppingCovid19$COSS, by = list(data_shoppingCovid19$SES_3cat), 
          FUN = mean, na.rm = TRUE, na.action = NULL)
aggregate(data_shoppingCovid19$COSS, by = list(data_shoppingCovid19$SES_3cat), 
          FUN = sd, na.rm = TRUE)

# SES group differences on OFFLINE compulsive buying
summary(aov(BERGEN ~ SES_3cat, data = data_shoppingCovid19))
TukeyHSD(aov(BERGEN ~ SES_3cat, data = data_shoppingCovid19))

# calculate mean and sd per group
aggregate(data_shoppingCovid19$BERGEN, by = list(data_shoppingCovid19$SES_3cat), 
          FUN = mean, na.rm = TRUE, na.action = NULL)
aggregate(data_shoppingCovid19$BERGEN, by = list(data_shoppingCovid19$SES_3cat), 
          FUN = sd, na.rm = TRUE)

# direct relationship between distress and ONLINE shopping
cor.test(data_shoppingCovid19$PSS, data_shoppingCovid19$COSS, method = "pearson")
# direct relationship between distress and OFFLINE shopping
cor.test(data_shoppingCovid19$PSS, data_shoppingCovid19$BERGEN, method = "pearson")

# SES and distress
summary(lm(PSS ~ SES_3cat, data = data_shoppingCovid19))
aggregate(data_shoppingCovid19$PSS, by = list(data_shoppingCovid19$SES_3cat), FUN = mean)
aggregate(data_shoppingCovid19$PSS, by = list(data_shoppingCovid19$SES_3cat), FUN = sd)
pairwise.t.test(data_shoppingCovid19$PSS, g = data_shoppingCovid19$SES_3cat, p.adjust.method = "bonferroni")

# SES and income
cor.test(as.numeric(data_shoppingCovid19$SES_subj), as.numeric(data_shoppingCovid19$income_now), method = "kendall")

# age and SES
cor.test(as.numeric(data_shoppingCovid19$SES_subj), as.numeric(data_shoppingCovid19$age), method = "kendall")

# age and income
cor.test(as.numeric(data_shoppingCovid19$income_now), as.numeric(data_shoppingCovid19$age), method = "kendall")


# GLM with max likelihood 

# make SES and income numeric
data_shoppingCovid19$SES_subj_num <- as.numeric(data_shoppingCovid19$SES_subj)
data_shoppingCovid19$income_now_num <- as.numeric(data_shoppingCovid19$income_now)

# online
summary(lm(COSS ~ PSS, data=data_shoppingCovid19))
summary(lm(COSS ~ PSS + SES_subj_num + income_now_num + age, data=data_shoppingCovid19))

#offline
summary(lm(BERGEN ~ PSS, data=data_shoppingCovid19))
summary(lm(BERGEN ~ PSS + SES_subj_num + income_now_num + age, data=data_shoppingCovid19))


#######################################################
## Aim 4: distress and compulsive buying in individuals with low SES / income vs. higher SES / income participants
#######################################################
# SES grouping
SES_groups <- c("POOR", "AVERAGE", "RICH")

# for online CB
for (i in SES_groups) {
  r = cor.test(~ COSS + PSS, data = data_shoppingCovid19, 
           subset = data_shoppingCovid19$SES_3cat == i, 
           method = "pearson", 
           conf.level = 0.95)
  print(i)
  print(r)
  } 

# for offline CB
for (i in SES_groups) {
  r = cor.test(~ BERGEN + PSS, data = data_shoppingCovid19, 
               subset = data_shoppingCovid19$SES_3cat == i, 
               method = "pearson", 
               conf.level = 0.95)
  print(i)
  print(r)
} 

# test differences between correlation coefficients
# assign income groups to "low", "medium" and "high" categories
data_shoppingCovid19$income_3cat <- recode_factor(data_shoppingCovid19$income_now, 
                                                  "under 15k" = "low", 
                                                  "15-25k" = "low",
                                                  "25-35k" = "low",
                                                  "35-50k" = "low", 
                                                  "50-75k" = "medium", 
                                                  "75-100k" = "high", 
                                                  "above 100k" = "high")
                                           

# run the correlation in each group according to income
income_groups <- c("low", "medium", "high", "Does not want to tell")

# online CB
for (i in income_groups) {
  r = cor.test(~ COSS + PSS, data = data_shoppingCovid19, 
         subset = data_shoppingCovid19$income_3cat == i, 
         method = "pearson", 
         conf.level = 0.95)
  print(i)
  print(r)
}

# offline CB
for (i in income_groups) {
  r = cor.test(~ BERGEN + PSS, data = data_shoppingCovid19, 
               subset = data_shoppingCovid19$income_3cat == i, 
               method = "pearson", 
               conf.level = 0.95)
  print(i)
  print(r)
}

# get Ns
# SES
table(is.na(data_shoppingCovid19$BERGEN), data_shoppingCovid19$SES_3cat)
table(is.na(data_shoppingCovid19$COSS), data_shoppingCovid19$SES_3cat)

# income
table(is.na(data_shoppingCovid19$BERGEN), data_shoppingCovid19$income_3cat)
table(is.na(data_shoppingCovid19$COSS), data_shoppingCovid19$income_3cat)


#####################################
# additional analyses finally not included in the paper
######################################

# MEDIATION with SES

# prep data because mediation does not handle missingness
data_shoppingCovid19_SESNoMiss <- subset(data_shoppingCovid19, !is.na(data_shoppingCovid19$SES_subj) )
data_shoppingCovid19_SESNoMiss <- subset(data_shoppingCovid19_SESNoMiss, !is.na(data_shoppingCovid19_SESNoMiss$COSS))
data_shoppingCovid19_SESNoMiss$SES_subj_num <- as.numeric(data_shoppingCovid19_SESNoMiss$SES_subj)

# online shopping
fit_medi <- lm(SES_subj_num ~ PSS, data = data_shoppingCovid19_SESNoMiss)
fit_online <- lm(COSS ~ PSS + SES_subj_num, data = data_shoppingCovid19_SESNoMiss) 
gvlma(fit_online) # for checking conditions of regression
fit_mediation_online <- mediate(fit_medi, fit_online, treat="PSS", mediator="SES_subj_num", sims = 50)
summary(fit_mediation_online)

# offline shopping
data_shoppingCovid19_SESNoMiss <- subset(data_shoppingCovid19_SESNoMiss, !is.na(data_shoppingCovid19_SESNoMiss$BERGEN))
fit_medi <- lm(SES_subj_num ~ PSS, data = data_shoppingCovid19_SESNoMiss)
fit_offline <- lm(BERGEN ~ PSS + SES_subj_num, data = data_shoppingCovid19_SESNoMiss) 
gvlma(fit_offline) # for checking conditions of regression
fit_mediation_offline <- mediate(fit_medi, fit_offline, treat="PSS", mediator="SES_subj_num", sims = 50)
summary(fit_mediation_offline)


# MEDIATION with income

# prep data because mediation does not handle missingness
data_shoppingCovid19_IncomeNoMiss <- subset(data_shoppingCovid19, !is.na(data_shoppingCovid19$income_now) )
data_shoppingCovid19_IncomeNoMiss <- subset(data_shoppingCovid19_IncomeNoMiss, !is.na(data_shoppingCovid19_IncomeNoMiss$COSS))
data_shoppingCovid19_IncomeNoMiss$Income_num <- as.numeric(data_shoppingCovid19_IncomeNoMiss$income_now)

# online shopping
fit_medi <- lm(Income_num ~ PSS, data = data_shoppingCovid19_IncomeNoMiss)
fit_online <- lm(COSS ~ PSS + Income_num, data = data_shoppingCovid19_IncomeNoMiss) 
gvlma(fit_online) # for checking conditions of regression
fit_mediation_online <- mediate(fit_medi, fit_online, treat="PSS", mediator="Income_num", sims = 50)
summary(fit_mediation_online)

# offline shopping
data_shoppingCovid19_IncomeNoMiss <- subset(data_shoppingCovid19_IncomeNoMiss, !is.na(data_shoppingCovid19_IncomeNoMiss$BERGEN))
fit_offline <- lm(BERGEN ~ PSS + Income_num, data = data_shoppingCovid19_IncomeNoMiss) 
gvlma(fit_online) # for checking conditions of regression
fit_mediation_online <- mediate(fit_medi, fit_online, treat="PSS", mediator="Income_num", sims = 50)
summary(fit_mediation_online)


