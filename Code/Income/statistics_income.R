#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", 
              "pspearman", "multcomp", "car", 
              "plyr")

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

# restrict to those who have made an online purchase in the past 7 days
data_shoppingCovid19_online <- subset(data_shoppingCovid19, online_purchase == "2") # meaning "yes"


#######################################################
## descriptive
#######################################################
# set data
data = data_shoppingCovid19_online

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
## prep data for H1
#######################################################
data_shoppingCovid19_online_income <- subset(data_shoppingCovid19_online, 
                                             select=c("SES_subj", "income_now", "income_before", "COSS", "age"))

# exclude "does not want to tell" data
data_shoppingCovid19_online_income <- subset(data_shoppingCovid19_online_income, income_now != "Does not \nwant to tell")
data_shoppingCovid19_online_income <- subset(data_shoppingCovid19_online_income, income_before != "Does not \nwant to tell")
data_shoppingCovid19_online_income$income_now <- factor(data_shoppingCovid19_online_income$income_now)
data_shoppingCovid19_online_income$income_before <- factor(data_shoppingCovid19_online_income$income_before)

# transform variables to numeric for the regression model
data_shoppingCovid19_online_income$income_now <- as.numeric(data_shoppingCovid19_online_income$income_now)
data_shoppingCovid19_online_income$income_before <- as.numeric(data_shoppingCovid19_online_income$income_before)

# transform SES data and exclude missing
data_shoppingCovid19_online_income <- subset(data_shoppingCovid19_online_income, !is.na(SES_subj))
data_shoppingCovid19_online_income$SES_subj <- as.numeric(data_shoppingCovid19_online_income$SES_subj)


#######################################################
## H1 testing: Compulsive buying is negatively associated with income and self-reported social status
#######################################################
# with income_now
lm_model_income_now <- lm(COSS ~ income_now + age, data=data_shoppingCovid19_online_income)
summary(lm_model_income_now)

# with income_before
lm_model_income_before <- lm(COSS ~ income_before + age, data=data_shoppingCovid19_online_income)
summary(lm_model_income_before)

# with SES_subj
lm_model_SES_subj <- lm(COSS ~ SES_subj + age, data=data_shoppingCovid19_online_income)
summary(lm_model_SES_subj)

# with income_now & SES_subj
lm_model_income_now_SES_subj <- lm(COSS ~ income_now + SES_subj + age, data=data_shoppingCovid19_online_income)
summary(lm_model_income_now_SES_subj)

AIC(lm_model_income_now_SES_subj)


#######################################################
## prep data for H2
#######################################################
data_shoppingCovid19_online_credit <- subset(data_shoppingCovid19_online, 
                                             select=c("age", "COSS", "CISS", 
                                                      "SES_subj", "income_now", "income_before", 
                                                      "debt", "debt_amount", 
                                                      "credit_card", "credit_card_amount"))

# exclude "does not want to tell" data
data_shoppingCovid19_online_credit <- subset(data_shoppingCovid19_online_credit, income_now != "Does not \nwant to tell")
data_shoppingCovid19_online_credit <- subset(data_shoppingCovid19_online_credit, income_before != "Does not \nwant to tell")

# transform VA for regression 
data_shoppingCovid19_online_credit$income_now <- as.numeric(factor(data_shoppingCovid19_online_credit$income_now))
data_shoppingCovid19_online_credit$income_before <- as.numeric(factor(data_shoppingCovid19_online_credit$income_before))
data_shoppingCovid19_online_credit$credit_card_amount <- factor(data_shoppingCovid19_online_credit$credit_card_amount)
data_shoppingCovid19_online_credit$debt_amount <- factor(data_shoppingCovid19_online_credit$debt_amount)
data_shoppingCovid19_online_credit$SES_subj <- as.numeric(data_shoppingCovid19_online_credit$SES_subj)

#######################################################
## H2 testing: compulsive buying is associated with greater credit card use and with higher amounts of unpaid balance
#######################################################
# company debt and shopping

# exclude missing data for the analysis
data_shoppingCovid19_online_credit_noMissDebt <- subset(data_shoppingCovid19_online_credit, debt_amount != "does not want to tell")

# test group differences (debt vs. no debt)
ttest_debt <- t.test(data_shoppingCovid19_online_credit_noMissDebt$COSS, data_shoppingCovid19_online_credit_noMissDebt$debt)
ttest_debt
ddply(data_shoppingCovid19_online_credit_noMissDebt, ~ debt, summarise, mean=mean(COSS), sd=sd(COSS), n=length(COSS))

# debt amount
lm_model_debt_amount <- lm(COSS ~ debt_amount, data=data_shoppingCovid19_online_credit_noMissDebt)
summary(lm_model_debt_amount)
ddply(data_shoppingCovid19_online_credit_noMissDebt, ~ debt_amount, summarise, mean=mean(COSS), sd=sd(COSS), n=length(COSS))

# credit and shopping

# exclude missing data for the analysis
data_shoppingCovid19_online_credit_noMissCredit <- subset(data_shoppingCovid19_online_credit, credit_card_amount != "does not want to tell")

# explore group differences (credit vs. no credit)
ttest_credit <- t.test(data_shoppingCovid19_online_credit_noMissCredit$COSS, data_shoppingCovid19_online_credit_noMissCredit$credit_card)
ttest_credit
ddply(data_shoppingCovid19_online_credit_noMissCredit, ~ credit_card, summarise, mean=mean(COSS), sd=sd(COSS), n=length(COSS))

# credit amount
lm_model_credit_amount <- lm(COSS ~ credit_card_amount, data=data_shoppingCovid19_online_credit_noMissCredit)
summary(lm_model_credit_amount)
ddply(data_shoppingCovid19_online_credit_noMissCredit, ~ credit_card_amount, summarise, mean=mean(COSS), sd=sd(COSS), n=length(COSS))


#######################################################
## H3 testing: given that compulsive buying is a psychological construct, distress is a better predictor than income or social status
#######################################################
lm_model_distress_SES <- lm(COSS ~ CISS + SES_subj, data=data_shoppingCovid19_online_credit)
summary(lm_model_distress_SES)

lm_model_distress_income <- lm(COSS ~ CISS + income_now, data=data_shoppingCovid19_online_credit)
summary(lm_model_distress_income)


#######################################################
## 4: explore the main spending categories during the course of the outbreak. 
#######################################################
# see visualisation


