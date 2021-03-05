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
              "mediation", "gvlma")

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
## prep data for H1
#######################################################
data_shoppingCovid19_income <- subset(data_shoppingCovid19, 
                                             select=c("SES_subj", "income_now", "income_before", "COSS", "BERGEN", "age"))

# exclude "does not want to tell" data
data_shoppingCovid19_income <- subset(data_shoppingCovid19_income, income_now != "Does not \nwant to tell")
data_shoppingCovid19_income <- subset(data_shoppingCovid19_income, income_before != "Does not \nwant to tell")
data_shoppingCovid19_income$income_now <- factor(data_shoppingCovid19_income$income_now)
data_shoppingCovid19_income$income_before <- factor(data_shoppingCovid19_income$income_before)

# transform variables to numeric for the regression model
data_shoppingCovid19_income$income_now <- as.numeric(data_shoppingCovid19_income$income_now)
data_shoppingCovid19_income$income_before <- as.numeric(data_shoppingCovid19_income$income_before)

# transform SES data and exclude missing
data_shoppingCovid19_income <- subset(data_shoppingCovid19_income, !is.na(SES_subj))
data_shoppingCovid19_income$SES_subj <- as.numeric(data_shoppingCovid19_income$SES_subj)


#######################################################
## H1 testing: Compulsive buying is negatively associated with income and self-reported social status
#######################################################
# with income_now ONLINE
summary(lm(COSS ~ income_now + age, data=data_shoppingCovid19_income))

# with income_now OFFLINE
summary(lm(BERGEN ~ income_now + age, data=data_shoppingCovid19_income))

# with SES_subj ONLINE
summary(lm(COSS ~ SES_subj + age, data=data_shoppingCovid19_income))

# with SES_subj OFFLINE
summary(lm(BERGEN ~ SES_subj + age, data=data_shoppingCovid19_income))
kötelező
# with SES_subj + income ONLINE
summary(lm(COSS ~ SES_subj + income_now + age, data=data_shoppingCovid19_income))

# with SES_subj OFFLINE
summary(lm(BERGEN ~ SES_subj + income_now + age, data=data_shoppingCovid19_income))

#######################################################
## prep data for H2
#######################################################
data_shoppingCovid19_credit <- subset(data_shoppingCovid19, 
                                             select=c("age", "COSS", "BERGEN", 
                                                      "SES_subj", "income_now", "income_before", 
                                                      "debt", "debt_amount", 
                                                      "credit_card", "credit_card_amount"))

# exclude "does not want to tell" data
data_shoppingCovid19_credit <- subset(data_shoppingCovid19_credit, income_now != "Does not \nwant to tell")
data_shoppingCovid19_credit <- subset(data_shoppingCovid19_credit, income_before != "Does not \nwant to tell")

# transform VA for regression 
data_shoppingCovid19_credit$income_now <- as.numeric(factor(data_shoppingCovid19_credit$income_now))
data_shoppingCovid19_credit$income_before <- as.numeric(factor(data_shoppingCovid19_credit$income_before))
data_shoppingCovid19_credit$credit_card_amount <- factor(data_shoppingCovid19_credit$credit_card_amount)
data_shoppingCovid19_credit$debt_amount <- factor(data_shoppingCovid19_credit$debt_amount)
data_shoppingCovid19_credit$SES_subj <- as.numeric(data_shoppingCovid19_credit$SES_subj)

#######################################################
## H2 testing: compulsive buying is associated with greater credit card use and with higher amounts of unpaid balance
#######################################################
# test group differences (debt vs. no debt)
#online
t.test(COSS ~ debt, data=data_shoppingCovid19)
# ddply(data_shoppingCovid19, ~ debt, summarise, mean=mean(COSS, na.rm = T), sd=sd(COSS, na.rm = T), n=length(COSS))
effectsize::cohens_d(COSS ~ debt, data = data_shoppingCovid19)
#offline
t.test(BERGEN ~ debt, data=data_shoppingCovid19)
effectsize::cohens_d(BERGEN ~ debt, data = data_shoppingCovid19)

# credit and shopping
# explore group differences (credit vs. no credit) ONLINE
t.test(data_shoppingCovid19$COSS, data_shoppingCovid19$credit_card)
effectsize::cohens_d(COSS ~ credit_card, data = data_shoppingCovid19)

# explore group differences (credit vs. no credit) ONLINE
t.test(data_shoppingCovid19$BERGEN, data_shoppingCovid19$credit_card)
effectsize::cohens_d(BERGEN ~ credit_card, data = data_shoppingCovid19)


## TESTING H2 - associations
# prep data - exclude those who did not have debt / unpaid credit card balance or did not want to tell this info
data_shoppingCovid19_debtNoMiss <- subset(data_shoppingCovid19, data_shoppingCovid19$debt_amount != "does not want to tell")
data_shoppingCovid19_creditNoMiss <- subset(data_shoppingCovid19, data_shoppingCovid19$credit_card_amount != "does not want to tell")

# debt amount ONLINE AND OFFLINE
cor.test(data_shoppingCovid19_debtNoMiss$COSS, as.numeric(data_shoppingCovid19_debtNoMiss$debt_amount), 
    method = "kendall")
cor.test(data_shoppingCovid19_debtNoMiss$BERGEN, as.numeric(data_shoppingCovid19_debtNoMiss$debt_amount), 
    method = "kendall")

# credit card amount ONLINE AND OFFLINE
cor.test(data_shoppingCovid19_creditNoMiss$COSS, as.numeric(data_shoppingCovid19_creditNoMiss$credit_card_amount), 
    method = "kendall")
cor.test(data_shoppingCovid19_creditNoMiss$BERGEN, as.numeric(data_shoppingCovid19_creditNoMiss$credit_card_amount), 
    method = "kendall")

#######################################################
## H3 testing: given that compulsive buying is a psychological construct, distress is a better predictor than income or social status
#######################################################
# GLM with max likelihood 
# online
summary(lm(COSS ~ PSS, data=data_shoppingCovid19))
summary(lm(COSS ~ PSS + SES_subj, data=data_shoppingCovid19))

#offline
summary(lm(BERGEN ~ PSS, data=data_shoppingCovid19))
summary(lm(BERGEN ~ PSS + SES_subj, data=data_shoppingCovid19))

#######################################################
#### outside of hypothesis-testing: mediation with SES / income 
#######################################################
# direct relationship between distress and ONLINE shopping
cor.test(data_shoppingCovid19$PSS, data_shoppingCovid19$COSS, method = "pearson")
# direct relationship between distress and OFFLINE shopping
cor.test(data_shoppingCovid19$PSS, data_shoppingCovid19$BERGEN, method = "pearson")

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


#######################################################
## 4: explore the main spending categories during the course of the outbreak. 
#######################################################
# see visualisation

#######################################################
## Overall regression model 
#######################################################
data_shoppingCovid19_regrNum <- na.omit(data_shoppingCovid19[, c("income_now", "SES_subj", "age", 
                                                                 "credit_card", "debt", "PSS", 
                                                                 "BERGEN", "COSS")])

# transform factors to numberic (levels are ordered)
data_shoppingCovid19_regrNum$income_num <- as.numeric(data_shoppingCovid19_regrNum$income_now)
data_shoppingCovid19_regrNum$SES_subj_num <- as.numeric(data_shoppingCovid19_regrNum$SES_subj)

# online
summary(lm(COSS ~ income_num + SES_subj_num + credit_card + debt + age + PSS, data=data_shoppingCovid19_regrNum))

# offline
summary(lm(BERGEN ~ income_num + SES_subj_num + credit_card + debt + age + PSS, data=data_shoppingCovid19_regrNum))

