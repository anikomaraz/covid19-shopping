#### load packages

library(tidyverse)
library(reshape2)
library(scales)
library(RColorBrewer)
library(cowplot)
library(ggpubr)

####load data
data_shoppingCovid19 <- read_rds(path="Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")

#descriptive stats
get_summary <- function(var, NARM = TRUE) {
  results <- c()
  FUN <- c("min", "max", "mean", "sd", "median")
  for (f in FUN) {
    res <- do.call(f, list(var, na.rm = NARM))
    results <- c(results, res)
  }
  names(results) <- c(FUN[1:2], "mean", "sd", FUN[5])
  results
}

#######COSS - compulsive buying online total###########

get_summary(data_shoppingCovid19$COSS)
table(data_shoppingCovid19$COSS, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS) +
  geom_histogram(bins = 15)

#######CoSS - salience###########
get_summary(data_shoppingCovid19$COSS_SALIENCE)
table(data_shoppingCovid19$COSS_SALIENCE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_SALIENCE) +
  geom_histogram(bins = 15)


######COSS - Mood modification##########
get_summary(data_shoppingCovid19$COSS_MOOD_MOD)
table(data_shoppingCovid19$COSS_MOOD_MOD, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_MOOD_MOD) +
  geom_histogram(bins = 15)


######COSS - CONFLICT##########
get_summary(data_shoppingCovid19$COSS_CONFLICT)
table(data_shoppingCovid19$COSS_CONFLICT, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_CONFLICT) +
  geom_histogram(bins = 15)

######COSS - TOLERANCE##########
get_summary(data_shoppingCovid19$COSS_TOLERANCE)
table(data_shoppingCovid19$COSS_TOLERANCE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_TOLERANCE) +
  geom_histogram(bins = 15)

######COSS - WITHDRAWAL##########
get_summary(data_shoppingCovid19$COSS_WITHDRAWAL)
table(data_shoppingCovid19$COSS_WITHDRAWAL, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_WITHDRAWAL) +
  geom_histogram(bins = 15)


######COSS - RELAPSE##########
get_summary(data_shoppingCovid19$COSS_RELAPSE)
table(data_shoppingCovid19$COSS_RELAPSE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_RELAPSE) +
  geom_histogram(bins = 15)

######COSS - PROBLEMS##########
get_summary(data_shoppingCovid19$COSS_PROBLEMS)
table(data_shoppingCovid19$COSS_PROBLEMS, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_PROBLEMS) +
  geom_histogram(bins = 15)

#######BERGEN - compulsive buying offline total###########

get_summary(data_shoppingCovid19$BERGEN)
table(data_shoppingCovid19$BERGEN, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN) +
  geom_histogram(bins = 15)

#######BERGEN - salience###########
get_summary(data_shoppingCovid19$BERGEN_SALIENCE)
table(data_shoppingCovid19$BERGEN_SALIENCE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_SALIENCE) +
  geom_histogram(bins = 15)


######BERGEN - Mood modification##########
get_summary(data_shoppingCovid19$BERGEN_MOOD_MOD)
table(data_shoppingCovid19$BERGEN_MOOD_MOD, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_MOOD_MOD) +
  geom_histogram(bins = 15)


######BERGEN - CONFLICT##########
get_summary(data_shoppingCovid19$BERGENS_CONFLICT)
table(data_shoppingCovid19$BERGENS_CONFLICT, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = COSS_CONFLICT) +
  geom_histogram(bins = 15)

######BERGEN - TOLERANCE##########
get_summary(data_shoppingCovid19$BERGEN_TOLERANCE)
table(data_shoppingCovid19$BERGEN_TOLERANCE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_TOLERANCE) +
  geom_histogram(bins = 15)

######BERGEN - WITHDRAWAL##########
get_summary(data_shoppingCovid19$BERGEN_WITHDRAWAL)
table(data_shoppingCovid19$BERGEN_WITHDRAWAL, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_WITHDRAWAL) +
  geom_histogram(bins = 15)


######BERGEN - RELAPSE##########
get_summary(data_shoppingCovid19$BERGEN_RELAPSE)
table(data_shoppingCovid19$BERGEN_RELAPSE, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_RELAPSE) +
  geom_histogram(bins = 15)

######BERGEN - PROBLEMS##########
get_summary(data_shoppingCovid19$BERGEN_PROBLEMS)
table(data_shoppingCovid19$BERGEN_PROBLEMS, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = BERGEN_PROBLEMS) +
  geom_histogram(bins = 15)


#######CISS_total############
get_summary(data_shoppingCovid19$CISS)
table(data_shoppingCovid19$CISS, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = CISS) +
  geom_histogram(bins = 15)


#######CISS_emotion############
get_summary(data_shoppingCovid19$CISS_EMOTION)
table(data_shoppingCovid19$CISS_EMOTION, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = CISS_EMOTION) +
  geom_histogram(bins = 15)

#######CISS_task############
get_summary(data_shoppingCovid19$CISS_TASK)
table(data_shoppingCovid19$CISS_TASK, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = CISS_TASK) +
  geom_histogram(bins = 15)


#######CISS_treat############
get_summary(data_shoppingCovid19$CISS_TREAT)
table(data_shoppingCovid19$CISS_TREAT, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = CISS_TREAT) +
  geom_histogram(bins = 10)


#######CISS_contact############
get_summary(data_shoppingCovid19$CISS_CONTACT)
table(data_shoppingCovid19$CISS_CONTACT, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = CISS_CONTACT) +
  geom_histogram(bins = 10)


##########PSS##############
get_summary(data_shoppingCovid19$PSS)
table(data_shoppingCovid19$PSS, useNA = "ifany")

data_shoppingCovid19 %>% 
  ggplot() +
  aes(x = PSS) +
  geom_histogram(bins = 15)



########gender####
table(data_shoppingCovid19$gender, useNA = "ifany")

#########age######
get_summary(data_shoppingCovid19$age)

############Chronbach's alpha############
#PSS
data_shoppingCovid19 %>% 
  select(pss_1:pss_14) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
      n.obs=NULL,impute=NULL)

#0.84

#COSS
data_shoppingCovid19 %>% 
  select(coss_salience_1:coss_problems_4) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.98 - akkor vszeg lehet az összpontszámot használni (bár gyanúsan magas...)

#BERGEN
data_shoppingCovid19 %>% 
  select(bergen_salience_1:bergen_problems_4) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.99 - akkor vszeg lehet az összpontszámot használni (bár gyanúsan magas ez is...)

#######CISS####
#CISS task
data_shoppingCovid19 %>% 
  select(ciss_1_task:ciss_7_task) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.83


#CISS emot
data_shoppingCovid19 %>% 
  select(ciss_8_emot:ciss_14_emot) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.88

#CISS treat
data_shoppingCovid19 %>% 
  select(ciss_15_treat:ciss_17_treat) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.78


#CISS contact
data_shoppingCovid19 %>% 
  select(ciss_18_contact:ciss_20_contact) %>% 
  psych::alpha(keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
               check.keys=TRUE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
               n.obs=NULL,impute=NULL)

#0.77

#########################correlations###############################

data_shoppingCovid19 %>% 
  select(COSS, BERGEN, PSS, CISS_TASK:CISS_CONTACT) %>%
  cor(use = "na.or.complete", method = "spearman")

#CISS_Treat has item = buy myself sg! need to omit if we want to use it & recalculate Chronbach's alpha, corr


####################model building######################

#install.packages("lavaan")
library(lavaan)

model1 <-' 
   # latent variables
     PSS =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_6 + pss_7 + pss_8 + pss_9 +  pss_10 + pss11 + pss_12 + pss_13 + pss_14
     #CISS_task =~ ciss_1_task+ciss_2_task+ciss_3_task+ciss_4_task+ciss_5_task+ciss_6_task+ciss_7_task
     CISS_emot =~ ciss_8_emot+ciss_9_emot+ciss_10_emot+ciss_11_emot+ciss_12_emot+ciss_13_emot+ciss_14_emot
     COSS =~ coss_salience_1 : coss_problems_4
     
   # regressions
     #CISS_task ~ PSS
     CISS_emot ~ PSS
     COSS ~ PSS
     COSS ~ PSS + CISS_emot
     #COSS ~ PSS + CISS_task
     
   # residual covariances (ezt nem annyira értem; https://lavaan.ugent.be/)
'
fit <- sem(model = model1,
           data  = data_shoppingCovid19)

summary(fit)

model2 <-' 
   # latent variables
     PSS =~ pss_1 + pss_2 + pss_3 + pss_4 + pss_5 + pss_6 + pss_7 + pss_8 + pss_9 +  pss_10 + pss11 + pss_12 + pss_13 + pss_14
     CISS_emot =~ ciss_8_emot+ciss_9_emot+ciss_10_emot+ciss_11_emot+ciss_12_emot+ciss_13_emot+ciss_14_emot
     COSS =~ coss_salience_1 : coss_problems_4
     
   # regressions
     CISS_emot ~ PSS
     COSS ~ PSS + CISS_emot
     
   # residual covariances (ezt nem annyira értem; https://lavaan.ugent.be/)
'
fit <- sem(model = model1,
           data  = data_shoppingCovid19)

summary(fit)








#missing data: FIML estimation...?
#control for gender, age & SES?
#COSS/BERGEN latent helyett parcelezve (7 alksálával)?



##################################APPENDIX - ITEMS#####################################
# ciss_intro_direkt	###When I encounter a difficult, stressful, or upsetting situation, I usually...
# ciss_1_task	**Focus on the problem and see how I can solve it.**
#   ciss_2_task	**Think about how I solved similar problems.**
#   ciss_3_task	**Determine a course of action and follow it.**
#   ciss_4_task	**Work to understand the situation.**
#   ciss_5_task	**Take corrective action immediately.**
#   ciss_6_task	**Think about the event and learn from my mistakes.**
#   ciss_7_task	**Analyze my problem before reacting.**
#   ciss_8_emot	**Blame myself for having gotten into this situation.**
#   ciss_9_emot	**Feel anxious about not being able to cope.**
#   ciss_10_emot	**Blame myself for being too emotional about the situation.**
#   ciss_11_emot	**Become very upset**
#   ciss_12_emot	**Blame myself for not knowing what to do.**
#   ciss_13_emot	**Wish that I could change what had happened or how I felt.**
#   ciss_14_emot	**Focus on my general inadequacies.**
#   ciss_15_treat	**Treat myself to a favorite food or snack.**
#   ciss_16_treat	**Buy myself something.**
#   ciss_17_treat	**Go out for a snack or meal.**
#   ciss_18_contact	**Visit a friend.** (when circumstances permit)
# ciss_19_contact	**Spend time with special person.** (when circumstances permit)
# ciss_20_contact	**Phone a friend**

# bergen_intro_direct	###In the past 30 days... 
# bergen_salience_1	**Shopping/buying was the most important thing in my life.**
#   bergen_salience_2	**I thought about shopping/buying things all the time.**
#   bergen_salience_3	**I spent a lot of time thinking of or planning shopping/buying.**
#   bergen_salience_4	**Thoughts about shopping/buying kept popping in my mind.**
#   bergen_moodMod_1	**Sometimes I shopped in order to feel better.**
#   bergen_moodMod_2	**Sometimes I shopped/bought things in order to change my mood.**
#   bergen_moodMod_3	** I shopped/bought things in order to forget about personal problems.**
#   bergen_moodMod_4	**I shopped/bought things in order to reduce feelings of guilt,anxiety, helplessness, loneliness, and/or depression.**
#   bergen_conflict_1	** I shopped/bought so much that it negatively affects my daily obligations (e.g., school and work).**
#   bergen_conflict_2	**I gave less priority to hobbies, leisure activities, job/studies, or exercise because of shopping/buying.**
#   bergen_conflict_3	**I had ignored to love partner, family, and friends because of shopping/buying.**
#   bergen_conflict_4	**I often ended up in arguments with other because of shopping/buying.**
#   bergen_tolerance_1	** I felt an increasing inclination to shop/buy things.**
#   bergen_tolerance_2	**I shopped/bought much more than I had intended/planned.**
#   bergen_tolerance_3	**I feel I had to shop/buy more and more to obtain the same satisfactions as before.**
#   bergen_tolerance_4	**I spent more and more time shopping/buying.**
#   bergen_relapse_1	**I tried to cut down on shopping/buying without success.**
#   bergen_relapse_2	**I have been told by others to reduce shopping/buying.**
#   bergen_relapse_3	**I decided to shop/buy less, but have not been able todo so.**
#   bergen_relapse_4	**I managed to limit shopping/buying for periods, and then experienced relapse.**
#   bergen_withdrawal_1	**I became stressed if obstructed from shopping/buying things.**
#   bergen_withdrawal_2	**I became sour and grumpy if I for some reasons cannot shop/buy things when I feel like it.**
#   bergen_withdrawal_3	**I felt bad if I for some reason I am prevented from shopping/buying things.**
#   bergen_withdrawal_4	**If it had been a while since I last shopped, I feel a strong urge to shop/buy things.**
#   bergen_problems_1	**I shopped/bought so much that it has caused economic problems.**
#   bergen_problems_2	**I shopped/bought so much that is has impaired my well-being.**
#   bergen_problems_3	**I worried so much about my shopping problems that it sometimes has made me sleepless.**
#   bergen_problems_4	**I had been bothered with poor conscience because of my shopping/buying.**
#   



# coss_intro_direct	###During the past 30 days...
# coss_salience_1	** Online shopping/buying was the most important thing in my life.**
#   coss_salience_2	**I thought about online shopping/buying things all the time.**
#   coss_salience_3	**I spent a lot of time thinking of or planning online shopping/buying.**
#   coss_salience_4	**Thoughts about online shopping/buying kept popping in my mind.**
#   coss_moodMod_1	**Sometimes I shopped online in order to feel better.**
#   coss_moodMod_2	**Sometimes I shopped/bought things online in order to change my mood.**
#   coss_moodMod_3	** I shopped/bought things online in order to forget about personal problems.**
#   coss_moodMod_4	**I shopped/bought things online in order to reduce feelings of guilt,anxiety, helplessness, loneliness, and/or depression.**
#   coss_conflict_1	** I shopped/bought online so much that it negatively affects my daily obligations (e.g., school and work).**
#   coss_conflict_2	**I gave less priority to hobbies, leisure activities, job/studies, or exercise because of online shopping/buying.**
#   coss_conflict_3	**I have ignored to love partner, family, and friends because of online shopping/buying.**
#   coss_conflict_4	**I often ended up in arguments with other because of online shopping/buying.**
#   coss_tolerance_1	** I felt an increasing inclination to shop/buy things online.**
#   coss_tolerance_2	**I shopped/bought online much more than I had intended/planned.**
#   coss_tolerance_3	**I feel I had to shop/buy more and more online to obtain the same satisfactions as before.**
#   coss_tolerance_4	**I spent more and more time shopping/buying online.**
#   coss_relapse_1	**I tried to cut down on online shopping/buying without success.**
#   coss_relapse_2	**I have been told by others to reduce online shopping/buying.**
#   coss_relapse_3	**I decided to shop/buy less online, but have not been able todo so.**
#   coss_relapse_4	**I managed to limit online shopping/buying for periods, and then experienced relapse.**
#   coss_withdrawal_1	**I became stressed if obstructed from shopping/buying things online.**
#   coss_withdrawal_2	**I became sour and grumpy if I for some reasons cannot shop/buy things online when I feel like it.**
#   coss_withdrawal_3	**I felt bad if I for some reason I am prevented from shopping/buying things online.**
#   coss_withdrawal_4	**If it had been a while since I last shopped online, I feelt a strong urge to shop/buy things.**
#   coss_problems_1	**I shopped/bought online so much that it has caused economic problems.**
#   coss_problems_2	**I shopped/bought online so much that is has impaired my well-being.**
#   coss_problems_3	**I worried so much about my online shopping problems that it sometimes has made me sleepless.**
#   coss_problems_4	**I have been bothered with poor conscience because of my online shopping/buying.**

  
# pss_1	**In the past 7 days, how often have you been upset because of something that happened unexpectedly?**
#   pss_2	**In the past 7 days, how often have you felt that you were unable to control the important things in your life?**
#   pss_3	**In the past 7 days, how often have you felt nervous and “stressed”?**
#   pss_4r	**In the past 7 days, how often have you dealt successfully with day to day problems and annoyances?**
#   pss_5r	**In the past 7 days, how often have you felt that you were effectively coping with important changes that were occurring in your life?**
#   pss_6r	**In the past 7 days, how often have you felt confident about your ability to handle your personal problems?**
#   pss_7r	**In the past 7 days, how often have you felt that things were going your way?**
#   pss_8	**In the past 7 days, how often have you found that you could not cope with all the things that you had to do?**
#   pss_9r	**In the past 7 days, how often have you been able to control irritations in your life?**
#   pss_10r	**In the past 7 days, how often have you felt that you were on top of things?**
#   pss_11	**In the past 7 days, how often have you been angered because of things that happened that were outside of your control?**
#   pss_12	**In the past 7 days, how often have you found yourself thinking about things that you have to accomplish?**
#   pss_13r	**In the past 7 days, how often have you been able to control the way you spend your time?**
#   pss_14	**In the past 7 days, how often have you felt difficulties were piling up so high that you could not overcome them?**
  
#social desirability - ture or false?
# sds_1r	**I'm always willing to admit it when I make a mistake.**
# sds_2r	**I always try to practice what I preach.**
# sds_3r	**I never resent being asked to return a favor.**
# sds_4r	**I have never been irked when people expressed ideas very differently from my own.**
# sds_5r	**I have never deliberately said something that hurt someone's feelings.**
#   sds_6	**I like to gossip at times.**
#   sds_7 **There have been occasions when I took advantage of someone.**
#   sds_8	**I sometimes try to get even rather than forgive and forget.**
#   sds_9	**At times I have really insisted on having things my own way. **
#   sds_10	**There have been occasions when I felt like smashing things. **
  

#SES_subj ### How wealthy do you think you are compared to others?
