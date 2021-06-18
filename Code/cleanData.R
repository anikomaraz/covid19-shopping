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
## read raw data and delete test cases
#######################################################

# read raw data
raw_shoppingCovid19 <- read.csv2("Data/survey_shopCovid_201002_final.csv", sep=",")

# basic summary of raw data
dim(raw_shoppingCovid19) -> data_read_in

# add time of filling out (batch series) based on the time stamp adding some rounding to end up with an integer batch no.
raw_shoppingCovid19 <- mutate(raw_shoppingCovid19, 
                              time_batch = round((as.numeric(as.POSIXct(strptime(created, format="%Y-%m-%d")))
                                                  - 1585177200) / (24*60*60) /3) +1)
raw_shoppingCovid19$time_batch <- as.factor(raw_shoppingCovid19$time_batch)
table(raw_shoppingCovid19$time_batch)

# calculate days since the Trump administration declared a national emergency on March 13
# the first data was collected on March 26
raw_shoppingCovid19$time_batch <- as.integer(raw_shoppingCovid19$time_batch)
raw_shoppingCovid19$time_days <- raw_shoppingCovid19$time_batch * 3 + 11
table(raw_shoppingCovid19$time_days)                                                                                   
                                                                                   
# delete test cases
test_cases <- "cleanSunBearXXX8rLbOARl9iNgo9YOvPZK4xptF8rbCyRCTGZeHL8-MXLiyIMEF"
raw_shoppingCovid19 <- raw_shoppingCovid19[raw_shoppingCovid19$session != test_cases, ]

# check N
dim(raw_shoppingCovid19) -> data_no_test_cases
data_no_test_cases

# delete incomplete cases
raw_shoppingCovid19 <- raw_shoppingCovid19[!is.na(raw_shoppingCovid19$age2), ] # $age2 was the last variable in the dataset

# check N
dim(raw_shoppingCovid19) -> data_no_incomplete
data_no_incomplete

# check time required to fill out the questionnaire (note that formr time measuring is not 100% reliable)
raw_shoppingCovid19 <- mutate(raw_shoppingCovid19, 
                              time_fill_out = strptime(ended, format="%Y-%m-%d %H:%M:%S") - strptime(created, format="%Y-%m-%d %H:%M:%S"))

which(raw_shoppingCovid19$time_fill_out > 100 )

# plot fill out time
ggplot(subset(raw_shoppingCovid19, time_fill_out < 60), aes(time_fill_out)) +
  geom_histogram(binwidth = 0.3) +
  labs(title="Time needed to fill out survey (minutes)") +
  theme_bw()

# check distribution of data "on the go"
median(raw_shoppingCovid19$time_fill_out, na.rm=T)
mean(raw_shoppingCovid19$time_fill_out, na.rm=T)
sd(raw_shoppingCovid19$time_fill_out, na.rm=T)


#######################################################
## attention check
#######################################################
# disregard missing data on attention_check_1, because this was not an obligatory item (take it as 0)
raw_shoppingCovid19$attention_check_1_noNA <- replace_na(raw_shoppingCovid19$attention_check_1, value = 5)

# calculate correct answers on the 4 attention check items (3 + age asked twice)
raw_shoppingCovid19 <- mutate(raw_shoppingCovid19, 
                              attention_check_correct = 
                                (attention_check_1_noNA == "5") + (attention_check_2 == "1") + (attention_check_3 == "2") +
                                (age == age2) + (age == age2 +1) + (age == age2 -1))

# create an error variable
raw_shoppingCovid19 <- mutate(raw_shoppingCovid19, 
                                                    attention_check_error = (4 - attention_check_correct))

# display attention check errors
table(raw_shoppingCovid19$attention_check_error)

# exclude those with more than 1 errors
raw_shoppingCovid19 <- raw_shoppingCovid19[raw_shoppingCovid19$attention_check_error < 2, ]

dim(raw_shoppingCovid19) -> data_no_excess_errors
data_no_excess_errors

#######################################################
## calculate lie scale
#######################################################
# SDS, the "lie" scale, where 1 = false, 2 = true
# first reverse items
raw_shoppingCovid19$sds_1 <- 3 - raw_shoppingCovid19$sds_1r
raw_shoppingCovid19$sds_2 <- 3 - raw_shoppingCovid19$sds_2r
raw_shoppingCovid19$sds_3 <- 3 - raw_shoppingCovid19$sds_3r
raw_shoppingCovid19$sds_4 <- 3 - raw_shoppingCovid19$sds_4r
raw_shoppingCovid19$sds_5 <- 3 - raw_shoppingCovid19$sds_5r

# calculate scale, where low scores mean that the participant is "too good to be true"
raw_shoppingCovid19$SDS_honesty <- rowSums(raw_shoppingCovid19[, grep("sds_\\d+$", colnames(raw_shoppingCovid19), value=T)], na.rm=F)

# exclude participants where lie scale is too high
raw_shoppingCovid19 <- raw_shoppingCovid19[raw_shoppingCovid19$SDS_honesty > 10, ]
dim(raw_shoppingCovid19) -> no_liers


#######################################################
## summary of cleaning and final data
#######################################################

excluded_data <- list(
  "raw data read in" = data_read_in, 
  "data without the test cases" = data_no_test_cases, 
  "data without incomplete cases (on age2)" = data_no_incomplete, 
  "data without participants making too many errors (>1)" = data_no_excess_errors, 
  "data without liers" = no_liers)
excluded_data

# save final data
write_rds(raw_shoppingCovid19, path="Data/raw_shoppingCovid19_clean.rds")

# checking data on the go
levels(raw_shoppingCovid19[, "BAs_other_text"])
last(raw_shoppingCovid19["feedback"], 50)
excluded_data
table(raw_shoppingCovid19$time_days)

# descriptions
table(raw_shoppingCovid19$gender)
mean(raw_shoppingCovid19$age)
sd(raw_shoppingCovid19$age)
table(raw_shoppingCovid19$edu_highest)
levels(raw_shoppingCovid19$BAs_other_text)
