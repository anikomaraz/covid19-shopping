
#######################################################
## packages and version setup
#######################################################
# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "scales", "RColorBrewer", 
              # "cowplot", 
              "ggpubr", "tidyjson", 
              "Cairo")

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
table(data_shoppingCovid19$time_batch)

## read Covid-19 cases data
## downloaded from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# create a new dataset (called data_covidCases) restricted to the time period of 26.03.2020 to 02.10.2020 only where countryterritoryCode == "USA", with the variables of "cases", "deaths", "popData2019" and "Cumulative_number_for..."
# create a new time variable (called "date" where day, month and year are merged and labelled as a date variable (use lubridate package), for example as 26.03.2020)
# create a new variable (based on date, call it "days_passed") which indicates how many days have passed since the start of the outbreak, which is officially the 13.03.2020
# save the downloaded and the extracted dataset to the "Data" folder
## (Eva)

#creat dataset data_covidCases
library(readxl)
library(dplyr)
getwd()
Covid19geograpicdistirbutionww <- read_xlsx("Data/COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx")
data_covidCases <- Covid19geograpicdistirbutionww %>% 
  filter(countryterritoryCode == "USA") %>% 
  filter(dateRep > "2020-03-26") %>%
  filter(dateRep < "2020-10-03") 

min(data_covidCases$dateRep)
max(data_covidCases$dateRep)

data_covidCases <- data_covidCases %>% 
 select(dateRep, countryterritoryCode, cases, deaths, popData2019, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)

# create a new time variable: new formate of date 26.03.2020
library(lubridate)
date <- strptime(as.character(data_covidCases$dateRep), "%Y-%m-%d")
date <- format(date, "%d.%m.%Y")

data_covidCases <- data.frame(data_covidCases, date)
data_covidCases <- data_covidCases %>% 
  select(date, countryterritoryCode, cases, deaths, popData2019, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)

#create a new variable "days_passed": 
outbreak <- c("13.03.2020")
data_covidCases <- data.frame(data_covidCases, outbreak)
days_passed <- as.Date(as.character(data_covidCases$date), format="%d.%m.%Y")-
  as.Date(as.character(data_covidCases$outbreak), format="%d.%m.%Y")
days_passed <- as.numeric(days_passed)

data_covidCases <- data_covidCases %>% 
  select(date, countryterritoryCode, cases, deaths, popData2019, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)

data_covidCases <- data.frame(data_covidCases, days_passed)

# check dataframe
ggplot(data_covidCases, aes(y=cases, x=days_passed)) + geom_line()

#save data_covidCases
write.csv(data_covidCases, file ="Data/data_covidCases.csv")

#######################################################
## settings for plots 
#######################################################
BA_colors8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# ,  "#FF9966"
# , "#999999"

span = 0.2

#######################################################
## BAs
#######################################################
data_behav_addictions <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq")]

data_behav_addictions_gather <- gather(data_behav_addictions, BA)
data_behav_addictions_gather$BA <- as.factor(data_behav_addictions_gather$BA)

data_behav_addictions_gather$BA <- factor(data_behav_addictions_gather$BA, 
                                          levels=c("BAs_gambling", "BAs_alcohol", "BAs_gaming", "BAs_illegal_drug",
                                                   "BAs_legal_drug", "BAs_other_freq", "BAs_overeating", "BAs_shopping", "BAs_smoking"), 
                                          labels=c("Gambling", "Alcohol", "Gaming", "Illegal substance(s)","Legal substance(s)",
                                                   "Other", "Overeating", "Shopping", "Smoking"))
data_behav_addictions_gather$BA <- fct_relevel(data_behav_addictions_gather$BA, 
                                               "Other", "Gaming", "Shopping","Overeating","Smoking", "Legal substance(s)", "Gambling",  
                                              "Illegal substance(s)", "Alcohol")
data_behav_addictions_gather$value <- as.factor(data_behav_addictions_gather$value)
data_behav_addictions_gather$value <- factor(data_behav_addictions_gather$value, 
                                             levels=c("not at all", "a little bit","somewhat", "quite a lot", 
                                                                                          "too much"))
data_behav_addictions_gather <- data_behav_addictions_gather[!is.na(data_behav_addictions_gather$value), ]


# plot
ggplot(data_behav_addictions_gather, aes(x=BA, fill=value)) +
  geom_bar(stat="count", position = "fill") +
  scale_fill_brewer(type="seq", palette="Oranges", na.value="grey80") +
  coord_flip() +
  labs(title="How often do you engage in the following behaviours?", y="Proportion", x="", fill="") +
  scale_y_continuous(n.breaks = 10) +
  theme(legend.position="top") + theme_pubr()


# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/BAs.png", 
       units="cm", width=20) 


#######################################################
## BA = mean over time
#######################################################
# prep data
BAs <- c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
         "BAs_gambling", "BAs_gaming", "BAs_overeating")
data_behav_addictions_mean_time <- data_shoppingCovid19[, c(BAs, "time_days")]

# transform BA variables to numeric
data_behav_addictions_mean_time[BAs] <- sapply(data_behav_addictions_mean_time[BAs],
                                               as.numeric)

# melt data
data_behav_addictions_mean_time_melt <- reshape2::melt(data_behav_addictions_mean_time, id.vars="time_days")

# recode BA categories
levels(data_behav_addictions_mean_time_melt$variable) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                           "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                           "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating")
# prep distress/CovidStress data
data_distress <- data_shoppingCovid19[, c("PSS", "stress_outbreak", "time_days")]
# scaling for comparability
data_distress$PSS_cent <- scale(data_distress$PSS, center=T, scale=T)
data_distress$stress_outbreak_cent <- scale(data_distress$stress_outbreak, center=T, scale=T)

# data_distress15$PSS <- (data_distress15$PSS - min(data_distress15$PSS)) / max(data_distress15$PSS) * 5 +2
# data_distress15$stress_outbreak_15 <- (data_distress15$stress_outbreak - min(data_distress15$stress_outbreak)) / max(data_distress15$stress_outbreak) * 5 +2

## Covid19 events in US economy
# start: 13.03.2020   President Trump issues the Proclamation on Declaring a National Emergency Concerning the Novel Coronavirus Disease (COVID-19) Outbreak, declaring a national state of emergency.[
# 27/03 Trump’s stimulus package # President Trump signs a $2 trillion stimulus package into law 
# The bill provides a one time payment of  a $1,200 check for individuals making up to $75,000 per year or $2,400 for couples earning less than $150,000. It also includes loans to businesses, funds unemployment insurance, bails out airlines and cargo carriers, authorizes aid to states and defers taxes, among other things
event_stimulus_package_day <- 27 # 09.04.2020
event_stimulus_package_name <- "Stimulus Package of $1,200"

# stay-at-home orders lifted # between 26 April and 13 May
event_lockdown_lifted_day <- 44 # 26.04.2020
event_lockdown_lifted_name <- "Lockdown lifting begins"

# The George Floyd protests begin in Minneapolis
event_GeorgeFloyd_day <-  74 # 26.05.2020
event_GeorgeFloyd_name <- "George Floyd protests begin"

# The House Appropriations Committee approved a measure requiring masks on public transportation
event_mask_day <- 123 # 14.07.2020
event_mask_name <- "Masks on public transport"

# July 28: The CDC calls for reopening American schools
event_schools_day <-  137 # 28.07.2020
event_schools_name <- "CDS calls for reopening\n  schools"


library(ggplot2)
# plot excessive behaviours
plot_BAs <- 
ggplot() +
  geom_smooth(method = "loess", data=data_behav_addictions_mean_time_melt,
              aes(x=time_days, y=value,
                  group = variable, color = variable),
              size = 0.7, 
              span = span, se=F)+
  scale_color_manual(values=BA_colors8) +
  labs(x="",
       y="Frequency",
       color="", group = "",
       title="How often did you engage in [name of the activity] in the past \n7 days?") +
  scale_fill_manual(name = "", values = c(BA_colors8, "DISTRESS" = "grey15", "COVID19 STRESS" = "purple")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  theme_pubr(legend="top") 

# plot distress 
plot_distress <- 
ggplot() +
  geom_smooth(method= "loess", data=data_distress, 
              aes(y=PSS_cent, x=time_days, fill="Distress (PSS)"), 
              linetype = "solid", color="darkred", 
              span = span) +
  geom_smooth(method = "loess", data=data_distress, 
              aes(y=stress_outbreak_cent, x=time_days, fill="Covid-19 stress (single item)"), 
              linetype="dashed", color="black", 
              span = span) +
  
  # scale_fill_manual(values=BA_colors8) +
  scale_color_manual(values=BA_colors8) +
  labs(x="",
       y="Z-scores",
       color="", group = "",
       title="Distress during the outbreak") +
  scale_fill_manual(name = "", values = c(BA_colors8, "Distress (PSS)" = "grey80", "Covid-19 stress (single item)" = "darkred")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  theme_pubr(legend="top")


# plot Covid19 cases
plot_cases <- 
  ggplot() +
    # add Covid cases
  geom_smooth(data = data_covidCases, aes(y = cases, x = days_passed), 
              color = "black", method = "loess", se=F) +
  labs(title="New Covid-19 cases", 
       x= "Time (days since the outbreak)\n between 26/03/2020 and 02/10/2020") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  scale_y_continuous(n.breaks = 8) +
  # add Covid19 events
  annotate(geom="text", x=event_stimulus_package_day + 20, y=50000, label=event_stimulus_package_name,
           color="red") +
  annotate(geom="text", x=event_lockdown_lifted_day + 21, y=40000, label=event_lockdown_lifted_name,
           color="red") +
  annotate(geom="text", x=event_mask_day + 15, y=73000, label=event_mask_name,
           color="red") +
  annotate(geom="text", x=event_GeorgeFloyd_day + 15, y=30000, label=event_GeorgeFloyd_name,
           color="red") +
  annotate(geom="text", x=event_schools_day + 32, y=63000, label=event_schools_name,
           color="red") +
  
  # add arrows
  geom_segment(aes(x = event_stimulus_package_day, y = 48000, 
                   xend = event_stimulus_package_day + 0.5, yend = 43000),
               arrow = arrow(length = unit(0.3, "cm")), color="red") + 
  geom_segment(aes(x = event_lockdown_lifted_day, y = 38000, 
                   xend = event_lockdown_lifted_day + 0.5, yend = 33000),
               arrow = arrow(length = unit(0.3, "cm")), color="red") + 
  geom_segment(aes(x = event_mask_day, y = 71000, 
                   xend = event_mask_day + 0.5, yend = 66000),
               arrow = arrow(length = unit(0.3, "cm")), color="red") +
  geom_segment(aes(x = event_GeorgeFloyd_day, y = 28000, 
                   xend = event_GeorgeFloyd_day + 0.5, yend = 23000),
               arrow = arrow(length = unit(0.3, "cm")), color="red") +
  geom_segment(aes(x = event_schools_day, y = 66000, 
                   xend = event_schools_day + 0.5, yend = 61000),
               arrow = arrow(length = unit(0.3, "cm")), color="red") +
  theme_pubr(legend="right")

# merge plot with facets
ggarrange(plot_BAs, plot_distress, plot_cases, ncol=1, nrow=3, 
          common.legend = F, align = "v")

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/BAs_all_perTime_facets.png", 
       width=18, height=27, units="cm") 


## PLOT CORR BETWEEN DISTRESS/COVID19 STRESS AND BEHAV.ADDICTIONS
# grep data from statistics file
source("Code/BAs/statistics_BAs.R")
# prep for plotting
data_corr_stressType_melt <- melt(data_corr_stressType, id=c("stress_type", "time_days"), 
                                  value.name="value", na.rm=T)
# apply Fisher's Z tranformation
data_corr_stressType_melt$value <- fisherz(data_corr_stressType_melt$value)

# prep variables for plotting
levels(data_corr_stressType_melt$variable) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                           "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                           "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating")

# plot correlations with PSS by BA type

ggplot() + 
  geom_smooth(data=data_corr_stressType_melt[data_corr_stressType_melt$stress_type == "PSS", ], # "1" = "stress_outbreak", "2" = "PSS"
              aes(x=time_days, y=value, 
                  color=variable)) +
  labs(x="Time (days passed since the outbreak)\n", 
       y="Correlation coefficient\n between distress and frequency of behaviour\n averaged for each timepoint", 
       color = "", 
       title = "Distress (PSS)") +
  scale_color_manual(values=BA_colors8) +
  facet_wrap("variable", nrow=4) +
  theme_pubr()

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/corr_byBA_facets_pss.png", 
       device = CairoPNG) 


# plot correlations with PSS by BA type

ggplot() + 
  geom_smooth(data=data_corr_stressType_melt[data_corr_stressType_melt$stress_type == "1", ], # "1" = "stress_outbreak", "2" = "PSS"
              aes(x=time_days, y=value, 
                  color=variable)) +
  labs(x="Time (days passed since the outbreak)\n", 
       y="Correlation coefficient\n between Covid-19 stress and frequency of behaviour\n averaged for each timepoint", 
       color = "", 
       title = "Covid-19-related stress (1 item)") +
  scale_color_manual(values=BA_colors8) +
  facet_wrap("variable") +
  theme_pubr()

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/corr_byBA_facets_covid19.png", 
       width=18, height=16, units="cm") 





# EVA
# calculate Spearman correlation at each point of time between distress (PSS scale) and each behavioral addiction (variables starting with BAs_  but leave out the BAs_other) 
#make a nice new data.frame with all relevant variables for a great overview 
plotcorrdisandBA<- data.frame(as.numeric(data_shoppingCovid19$BAs_shopping), as.numeric(data_shoppingCovid19$BAs_alcohol), 
                              as.numeric(data_shoppingCovid19$BAs_smoking), as.numeric(data_shoppingCovid19$BAs_legal_drug),
                              as.numeric(data_shoppingCovid19$BAs_illegal_drug), as.numeric(data_shoppingCovid19$BAs_gambling),
                              as.numeric(data_shoppingCovid19$BAs_gaming), as.numeric(data_shoppingCovid19$BAs_overeating), 
                              as.numeric(data_shoppingCovid19$time_batch), data_shoppingCovid19$PSS, data_shoppingCovid19$stress_outbreak)

#rename colums
colnames(plotcorrdisandBA)
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_shopping."] <- "BAs_shopping"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_alcohol."] <- "BAs_alcohol"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_smoking."] <- "BAs_smoking"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_legal_drug."] <- "BAs_legal_drug"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_illegal_drug."] <- "BAs_illegal_drug"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_gambling."] <- "BAs_gambling"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_gaming."] <- "BAs_gaming"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.BAs_overeating."] <- "BAs_overeating"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "as.numeric.data_shoppingCovid19.time_batch."] <- "time_batch"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.PSS"] <- "PSS"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"
head(plotcorrdisandBA)

# Spearman Correlations between each behavioural problem and PSS
cor.test(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$PSS, method="spearman", na.rm = T) 

#separately: Covid-19 stress (stress_outbreak variable) and each behavioural addiction. In total 8 behavioural addictions, meaning 16 correlations at each point of time. 
cor.test(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)
cor.test(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$stress_outbreak, method="spearman", na.rm = T)

#create two plots, separately for distress, and for covid19 stress. The strength 
#of the correlation should be on the y axis (mean + SE or SD calculated for each assessment), 
#and time (days_passed) on the x axis, and of course a separate line for each behavioural addiction.
#calculate correlation between BAs and PSS 
#shopping and PSS
require(plyr)
funcshopping <- function(plotcorrdisandBA)
{
  return(data.frame(CORshopping = cor(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$PSS )))
}
ashopping <-ddply(plotcorrdisandBA, .(time_batch), funcshopping)

#alcohol and PSS
require(plyr)
funcalcohol <- function(plotcorrdisandBA)
{
  return(data.frame(CORalcohol = cor(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$PSS )))
}
aalcohol <-ddply(plotcorrdisandBA, .(time_batch), funcalcohol)

#smoking and PSS
require(plyr)
funcsmoking <- function(plotcorrdisandBA)
{
  return(data.frame(CORsmoking = cor(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$PSS )))
}
asmoking <-ddply(plotcorrdisandBA, .(time_batch), funcsmoking)

#legal drug and PSS
require(plyr)
funclegaldrug <- function(plotcorrdisandBA)
{
  return(data.frame(CORlegaldrug = cor(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$PSS )))
}
alegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funclegaldrug)

#illegal drug and PSS
require(plyr)
funcillegaldrug <- function(plotcorrdisandBA)
{
  return(data.frame(CORillegaldrug = cor(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$PSS )))
}
aillegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funcillegaldrug)

#gambling and PSS
require(plyr)
funcgambling <- function(plotcorrdisandBA)
{
  return(data.frame(CORgambling = cor(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$PSS )))
}
agambling <-ddply(plotcorrdisandBA, .(time_batch), funcgambling)

#gaming and PSS
require(plyr)
funcgaming <- function(plotcorrdisandBA)
{
  return(data.frame(CORgaming = cor(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$PSS )))
}
agaming <-ddply(plotcorrdisandBA, .(time_batch), funcgaming)

#overeating and PSS
require(plyr)
funcovereating <- function(plotcorrdisandBA)
{
  return(data.frame(CORovereating = cor(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$PSS )))
}
aovereating <-ddply(plotcorrdisandBA, .(time_batch), funcovereating)

#make a dataframe for H1: Correlation BAs and PSS
H1BAsPSS <- data.frame(ashopping, aalcohol$CORalcohol, asmoking$CORsmoking, alegaldrug$CORlegaldrug, aillegaldrug$CORillegaldrug , agambling$CORgambling, agaming$CORgaming, aovereating$CORovereating)
colnames(H1BAsPSS) #tidying the colnames
names(H1BAsPSS)[names(H1BAsPSS) == "aalcohol.CORalcohol"] <- "CORalcohol"
names(H1BAsPSS)[names(H1BAsPSS) == "asmoking.CORsmoking"] <- "CORsmoking"
names(H1BAsPSS)[names(H1BAsPSS) == "alegaldrug.CORlegaldrug"] <- "CORlegaldrug"
names(H1BAsPSS)[names(H1BAsPSS) == "aillegaldrug.CORillegaldrug"] <- "CORillegaldrug"
names(H1BAsPSS)[names(H1BAsPSS) == "agambling.CORgambling"] <- "CORgambling"
names(H1BAsPSS)[names(H1BAsPSS) == "agaming.CORgaming"] <- "CORgaming"
names(H1BAsPSS)[names(H1BAsPSS) == "aovereating.CORovereating"] <- "CORovereating"

library(psych)
describeBy(H1BAsPSS, group = H1BAsPSS$time_batch, na.rm = T)

#calculation means and sd für every BAs
library(plyr)
MeanSDBA <- ddply(H1BAsPSS, .(time_batch), summarize, MeanCORshopping=mean(CORshopping), SDCORshopping=sd(CORshopping), 
                MeanCORalcohol=mean(CORalcohol), SDCORalcohol=sd(CORalcohol), MeanCORsmoking=mean(CORsmoking), SDCORsmoking=sd(CORsmoking), 
                MeanCORlegaldrug=mean(CORlegaldrug), SDCORlegaldrug=sd(CORlegaldrug), MeanCORillegaldrug=mean(CORillegaldrug), SDCORillegaldrug=sd(CORillegaldrug),
                MeanCORgambling=mean(CORgambling), SDCORgambling=sd(CORgambling), MeanCORgaming=mean(CORgaming), SDCORgaming=sd(CORgaming), 
                MeanCORovereating=mean(CORovereating), SDCORovereating=sd(CORovereating))

#sorting data
MeanSDBAshopping <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORshopping, c("MeanCORshopping"), c("#333BFF"))
colnames(MeanSDBAshopping) #tidying the colnames
names(MeanSDBAshopping)[names(MeanSDBAshopping) == "MeanSDBA.MeanCORshopping"] <- "Mean"
names(MeanSDBAshopping)[names(MeanSDBAshopping) == "c..MeanCORshopping.."] <- "Name"
names(MeanSDBAshopping)[names(MeanSDBAshopping) == "c...333BFF.."] <- "cyl"

MeanSDBAalcohol <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORalcohol, c("MeanCORalcohol"), c("#CC6600"))
colnames(MeanSDBAalcohol) #tidying the colnames
names(MeanSDBAalcohol)[names(MeanSDBAalcohol) == "MeanSDBA.MeanCORalcohol"] <- "Mean"
names(MeanSDBAalcohol)[names(MeanSDBAalcohol) == "c..MeanCORalcohol.."] <- "Name"
names(MeanSDBAalcohol)[names(MeanSDBAalcohol) == "c...CC6600.."] <- "cyl"

MeanSDBAsmoking <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORsmoking, c("MeanCORsmoking"))
colnames(MeanSDBAsmoking) #tidying the colnames
names(MeanSDBAsmoking)[names(MeanSDBAsmoking) == "MeanSDBA.MeanCORsmoking"] <- "Mean"
names(MeanSDBAsmoking)[names(MeanSDBAsmoking) == "c..MeanCORsmoking.."] <- "Name"

MeanSDBAlegaldrug <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORlegaldrug, c("MeanCORlegaldrug"))
colnames(MeanSDBAlegaldrug) #tidying the colnames
names(MeanSDBAlegaldrug)[names(MeanSDBAlegaldrug) == "MeanSDBA.MeanCORlegaldrug"] <- "Mean"
names(MeanSDBAlegaldrug)[names(MeanSDBAlegaldrug) == "c..MeanCORlegaldrug.."] <- "Name"

MeanSDBAillegaldrug <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORillegaldrug, c("MeanCORillegaldrug"))
colnames(MeanSDBAillegaldrug) #tidying the colnames
names(MeanSDBAillegaldrug)[names(MeanSDBAillegaldrug) == "MeanSDBA.MeanCORillegaldrug"] <- "Mean"
names(MeanSDBAillegaldrug)[names(MeanSDBAillegaldrug) == "c..MeanCORillegaldrug.."] <- "Name"

MeanSDBAgambling <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORgambling, c("MeanCORgambling"))
colnames(MeanSDBAgambling) #tidying the colnames
names(MeanSDBAgambling)[names(MeanSDBAgambling) == "MeanSDBA.MeanCORgambling"] <- "Mean"
names(MeanSDBAgambling)[names(MeanSDBAgambling) == "c..MeanCORgambling.."] <- "Name"

MeanSDBAgaming <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORgaming, c("MeanCORgaming"))
colnames(MeanSDBAgaming) #tidying the colnames
names(MeanSDBAgaming)[names(MeanSDBAgaming) == "MeanSDBA.MeanCORgaming"] <- "Mean"
names(MeanSDBAgaming)[names(MeanSDBAgaming) == "c..MeanCORgaming.."] <- "Name"

MeanSDBAovereating <- data.frame(MeanSDBA$time_batch, MeanSDBA$MeanCORovereating, c("MeanCORovereating"))
colnames(MeanSDBAovereating) #tidying the colnames
names(MeanSDBAovereating)[names(MeanSDBAovereating) == "MeanSDBA.MeanCORovereating"] <- "Mean"
names(MeanSDBAovereating)[names(MeanSDBAovereating) == "c..MeanCORovereating.."] <- "Name"

#merge all subframes into a dataframe 
library(dplyr)
library(tidyjson)
MeanSDBAfinish <- bind_rows(MeanSDBAalcohol, MeanSDBAshopping, MeanSDBAsmoking, MeanSDBAlegaldrug, MeanSDBAillegaldrug, MeanSDBAgambling, MeanSDBAgaming, MeanSDBAovereating)
colnames(MeanSDBAfinish) #tidying the colnames
names(MeanSDBAfinish)[names(MeanSDBAfinish) == "MeanSDBA.time_batch"] <- "time_batch"

#plot: x-asis time_batch; y-asis Mean of the Correlation (BAs and PSS)
library(plotly)
pfinish <- ggplot(MeanSDBAfinish, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figfinish <- ggplotly(pfinish)
figfinish #it looks too messy --> merge two BAs into a plot 

#merge two subframes into a dataframe 
#BA shopping and alcohol
MeanSDBAshoppingalcohol <- bind_rows(MeanSDBAalcohol, MeanSDBAshopping)
colnames(MeanSDBAshoppingalcohol) #tidying the colnames
names(MeanSDBAshoppingalcohol)[names(MeanSDBAshoppingalcohol) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA smoking and overeating
MeanSDBAsmokingovereating <- bind_rows(MeanSDBAsmoking, MeanSDBAovereating)
colnames(MeanSDBAsmokingovereating) #tidying the colnames
names(MeanSDBAsmokingovereating)[names(MeanSDBAsmokingovereating) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA legal and illegal drug
MeanSDBAlegalillegaldrug <- bind_rows(MeanSDBAlegaldrug, MeanSDBAillegaldrug)
colnames(MeanSDBAlegalillegaldrug) #tidying the colnames
names(MeanSDBAlegalillegaldrug)[names(MeanSDBAlegalillegaldrug) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA gambling and gaming
MeanSDBAgamblinggaming <- bind_rows(MeanSDBAgambling, MeanSDBAgaming)
colnames(MeanSDBAgamblinggaming) #tidying the colnames
names(MeanSDBAgamblinggaming)[names(MeanSDBAgamblinggaming) == "MeanSDBA.time_batch"] <- "time_batch"


#plot: x-asis time_batch; y-asis Mean of the Correlation (BAs and PSS) 
#BA alcohol and shopping into one
pshoppingalcohol <- ggplot(MeanSDBAshoppingalcohol, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figshoppingalcohol <- ggplotly(pshoppingalcohol)
figshoppingalcohol

#BA smoking and overeating into one
psmokingovereating <- ggplot(MeanSDBAsmokingovereating, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figsmokingovereating <- ggplotly(psmokingovereating)
figsmokingovereating

#BA legal and illegal drug 
plegalillegaldrug <- ggplot(MeanSDBAlegalillegaldrug, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figlegalillegaldrug <- ggplotly(plegalillegaldrug)
figlegalillegaldrug

#BA gambling and gaming
pgamblinggaming <- ggplot(MeanSDBAgamblinggaming, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figgamblinggaming <- ggplotly(pgamblinggaming)
figgamblinggaming

#calculate correlation between BAs and stress_outbreak
#shoppingand stress_outbreak
require(plyr)
funcshoppingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORshopping = cor(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$stress_outbreak)))
}
bshopping <-ddply(plotcorrdisandBA, .(time_batch), funcshoppingb)

#alcohol and stress_outbreak
require(plyr)
funcalcoholb <- function(plotcorrdisandBA)
{
  return(data.frame(CORalcohol = cor(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$stress_outbreak)))
}
balcohol <-ddply(plotcorrdisandBA, .(time_batch), funcalcoholb)

#smoking and stress_outbreak
require(plyr)
funcsmokingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORsmoking = cor(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$stress_outbreak)))
}
bsmoking <-ddply(plotcorrdisandBA, .(time_batch), funcsmokingb)

#legal drug and stress_outbreak
require(plyr)
funclegaldrugb <- function(plotcorrdisandBA)
{
  return(data.frame(CORlegaldrug = cor(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$stress_outbreak)))
}
blegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funclegaldrugb)

#illegal drug and stress_outbreak
require(plyr)
funcillegaldrugb <- function(plotcorrdisandBA)
{
  return(data.frame(CORillegaldrug = cor(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$stress_outbreak)))
}
billegaldrug <-ddply(plotcorrdisandBA, .(time_batch), funcillegaldrugb)

#gambling and stress_outbreak
require(plyr)
funcgamblingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORgambling = cor(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$stress_outbreak)))
}
bgambling <-ddply(plotcorrdisandBA, .(time_batch), funcgamblingb)

#gaming and stress_outbreak
require(plyr)
funcgamingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORgaming = cor(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$stress_outbreak)))
}
bgaming <-ddply(plotcorrdisandBA, .(time_batch), funcgamingb)

#overeating and stress_outbreak
require(plyr)
funcovereatingb <- function(plotcorrdisandBA)
{
  return(data.frame(CORovereating = cor(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$stress_outbreak)))
}
bovereating <-ddply(plotcorrdisandBA, .(time_batch), funcovereatingb)

#make a dataframe for H1: Correlation BAs and stress_outbreak
H1BAsstressoutbreak <- data.frame(bshopping, balcohol$CORalcohol, bsmoking$CORsmoking, blegaldrug$CORlegaldrug, billegaldrug$CORillegaldrug , bgambling$CORgambling, bgaming$CORgaming, bovereating$CORovereating)
colnames(H1BAsstressoutbreak) #tidying the colnames
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "balcohol.CORalcohol"] <- "CORalcohol"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bsmoking.CORsmoking"] <- "CORsmoking"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "blegaldrug.CORlegaldrug"] <- "CORlegaldrug"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "billegaldrug.CORillegaldrug"] <- "CORillegaldrug"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bgambling.CORgambling"] <- "CORgambling"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bgaming.CORgaming"] <- "CORgaming"
names(H1BAsstressoutbreak)[names(H1BAsstressoutbreak) == "bovereating.CORovereating"] <- "CORovereating"

#calculation means and sd für every BAs
library(plyr)
MeanSDBAstress <- ddply(H1BAsstressoutbreak, .(time_batch), summarize, MeanCORshopping=mean(CORshopping), SDCORshopping=sd(CORshopping), 
                  MeanCORalcohol=mean(CORalcohol), SDCORalcohol=sd(CORalcohol), MeanCORsmoking=mean(CORsmoking), SDCORsmoking=sd(CORsmoking), 
                  MeanCORlegaldrug=mean(CORlegaldrug), SDCORlegaldrug=sd(CORlegaldrug), MeanCORillegaldrug=mean(CORillegaldrug), SDCORillegaldrug=sd(CORillegaldrug),
                  MeanCORgambling=mean(CORgambling), SDCORgambling=sd(CORgambling), MeanCORgaming=mean(CORgaming), SDCORgaming=sd(CORgaming), 
                  MeanCORovereating=mean(CORovereating), SDCORovereating=sd(CORovereating))

#sorting data
MeanSDBAstressshopping <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORshopping, c("MeanCORshopping"))
colnames(MeanSDBAstressshopping) #tidying the colnames
names(MeanSDBAstressshopping)[names(MeanSDBAstressshopping) == "MeanSDBAstress.MeanCORshopping"] <- "Mean"
names(MeanSDBAstressshopping)[names(MeanSDBAstressshopping) == "c..MeanCORshopping.."] <- "Name"
names(MeanSDBAstressshopping)[names(MeanSDBAstressshopping) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstressalcohol <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORalcohol, c("MeanCORalcohol"))
colnames(MeanSDBAstressalcohol) #tidying the colnames
names(MeanSDBAstressalcohol)[names(MeanSDBAstressalcohol) == "MeanSDBAstress.MeanCORalcohol"] <- "Mean"
names(MeanSDBAstressalcohol)[names(MeanSDBAstressalcohol) == "c..MeanCORalcohol.."] <- "Name"
names(MeanSDBAstressalcohol)[names(MeanSDBAstressalcohol) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstresssmoking <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORsmoking, c("MeanCORsmoking"))
colnames(MeanSDBAstresssmoking) #tidying the colnames
names(MeanSDBAstresssmoking)[names(MeanSDBAstresssmoking) == "MeanSDBAstress.MeanCORsmoking"] <- "Mean"
names(MeanSDBAstresssmoking)[names(MeanSDBAstresssmoking) == "c..MeanCORsmoking.."] <- "Name"
names(MeanSDBAstresssmoking)[names(MeanSDBAstresssmoking) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstresslegaldrug <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORlegaldrug, c("MeanCORlegaldrug"))
colnames(MeanSDBAstresslegaldrug) #tidying the colnames
names(MeanSDBAstresslegaldrug)[names(MeanSDBAstresslegaldrug) == "MeanSDBAstress.MeanCORlegaldrug"] <- "Mean"
names(MeanSDBAstresslegaldrug)[names(MeanSDBAstresslegaldrug) == "c..MeanCORlegaldrug.."] <- "Name"
names(MeanSDBAstresslegaldrug)[names(MeanSDBAstresslegaldrug) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstressillegaldrug <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORillegaldrug, c("MeanCORillegaldrug"))
colnames(MeanSDBAstressillegaldrug) #tidying the colnames
names(MeanSDBAstressillegaldrug)[names(MeanSDBAstressillegaldrug) == "MeanSDBAstress.MeanCORillegaldrug"] <- "Mean"
names(MeanSDBAstressillegaldrug)[names(MeanSDBAstressillegaldrug) == "c..MeanCORillegaldrug.."] <- "Name"
names(MeanSDBAstressillegaldrug)[names(MeanSDBAstressillegaldrug) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstressgambling <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORgambling, c("MeanCORgambling"))
colnames(MeanSDBAstressgambling) #tidying the colnames
names(MeanSDBAstressgambling)[names(MeanSDBAstressgambling) == "MeanSDBAstress.MeanCORgambling"] <- "Mean"
names(MeanSDBAstressgambling)[names(MeanSDBAstressgambling) == "c..MeanCORgambling.."] <- "Name"
names(MeanSDBAstressgambling)[names(MeanSDBAstressgambling) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstressgaming <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORgaming, c("MeanCORgaming"))
colnames(MeanSDBAstressgaming) #tidying the colnames
names(MeanSDBAstressgaming)[names(MeanSDBAstressgaming) == "MeanSDBAstress.MeanCORgaming"] <- "Mean"
names(MeanSDBAstressgaming)[names(MeanSDBAstressgaming) == "c..MeanCORgaming.."] <- "Name"
names(MeanSDBAstressgaming)[names(MeanSDBAstressgaming) == "MeanSDBAstress.time_batch"] <- "time_batch"

MeanSDBAstressovereating <- data.frame(MeanSDBAstress$time_batch, MeanSDBAstress$MeanCORovereating, c("MeanCORovereating"))
colnames(MeanSDBAstressovereating) #tidying the colnames
names(MeanSDBAstressovereating)[names(MeanSDBAstressovereating) == "MeanSDBAstress.MeanCORovereating"] <- "Mean"
names(MeanSDBAstressovereating)[names(MeanSDBAstressovereating) == "c..MeanCORovereating.."] <- "Name"
names(MeanSDBAstressovereating)[names(MeanSDBAstressovereating) == "MeanSDBAstress.time_batch"] <- "time_batch"

#merge all subframes into a dataframe 
MeanSDBAstressfinish <- bind_rows(MeanSDBAstressalcohol, MeanSDBAstressshopping, MeanSDBAstresssmoking, MeanSDBAstresslegaldrug, MeanSDBAstressillegaldrug, MeanSDBAstressgambling, MeanSDBAstressgaming, MeanSDBAstressovereating)
colnames(MeanSDBAstressfinish) #tidying the colnames
names(MeanSDBAstressfinish)[names(MeanSDBAstressfinish) == "MeanSDBA.time_batch"] <- "time_batch"

#plot: x-asis time_batch; y-asis Mean of the Correlation (BAs and stress_outbreak)
library(plotly)
pstressfinish <- ggplot(MeanSDBAstressfinish, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figstressfinish <- ggplotly(pstressfinish)
figstressfinish #it looks too messy --> merge two BAs into a plot 

#merge two subframes into a dataframe 
#BA shopping and alcohol
MeanSDBAstressshoppingalcohol <- bind_rows(MeanSDBAstressalcohol, MeanSDBAstressshopping)
colnames(MeanSDBAstressshoppingalcohol) #tidying the colnames
names(MeanSDBAstressshoppingalcohol)[names(MeanSDBAstressshoppingalcohol) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA smoking and overeating
MeanSDBAstresssmokingovereating <- bind_rows(MeanSDBAstresssmoking, MeanSDBAstressovereating)
colnames(MeanSDBAstresssmokingovereating) #tidying the colnames
names(MeanSDBAstresssmokingovereating)[names(MeanSDBAstresssmokingovereating) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA legal and illegal drug
MeanSDBAstresslegalillegaldrug <- bind_rows(MeanSDBAstresslegaldrug, MeanSDBAstressillegaldrug)
colnames(MeanSDBAstresslegalillegaldrug) #tidying the colnames
names(MeanSDBAstresslegalillegaldrug)[names(MeanSDBAstresslegalillegaldrug) == "MeanSDBA.time_batch"] <- "time_batch"

#merge two subframes into a dataframe 
#BA gambling and gaming
MeanSDBAstressgamblinggaming <- bind_rows(MeanSDBAstressgambling, MeanSDBAstressgaming)
colnames(MeanSDBAstressgamblinggaming) #tidying the colnames
names(MeanSDBAstressgamblinggaming)[names(MeanSDBAstressgamblinggaming) == "MeanSDBA.time_batch"] <- "time_batch"


#plot: x-asis time_batch; y-asis Mean of the Correlation (BAs and stress_outbreak) 
#BA alcohol and shopping into one
pstressshoppingalcohol <- ggplot(MeanSDBAstressshoppingalcohol, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figstressshoppingalcohol <- ggplotly(pstressshoppingalcohol)
figstressshoppingalcohol

#BA smoking and overeating into one
pstresssmokingovereating <- ggplot(MeanSDBAstresssmokingovereating, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figstresssmokingovereating <- ggplotly(pstresssmokingovereating)
figstresssmokingovereating

#BA legal and illegal drug 
pstresslegalillegaldrug <- ggplot(MeanSDBAstresslegalillegaldrug, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figstresslegalillegaldrug <- ggplotly(pstresslegalillegaldrug)
figstresslegalillegaldrug

#BA gambling and gaming
pstressgamblinggaming <- ggplot(MeanSDBAstressgamblinggaming, aes(x=time_batch, y=Mean, group=Name, color = Name, span = 0.3)) +
  geom_smooth(method = "lm")+
  geom_point() +
  scale_colour_discrete("BAs")
figstressgamblinggaming <- ggplotly(pstressgamblinggaming)
figstressgamblinggaming


# IGNORED PLOTS
#Plot with Eva's correlations 
plot <- MeanSDBAstressfinish 
colnames(plot) #tidying the colnames
names(plot)[names(plot) == "Name"] <- "variable"

ggplot() + 
  geom_smooth(data=plot, 
              aes(x=time_batch, y=Mean, 
                  color=variable)) +
  labs(x="Time (days passed since the outbreak)\n", 
       y="Correlation coefficient\n between distress and frequency of behaviour\n averaged for each timepoint", 
       color = "", 
       title = "Covid-19-related stress (1 item)") +
  scale_color_manual(values=BA_colors8) +
  facet_wrap("variable") +
  theme_pubr()

#######################################################
## BA = "too much" over time
#######################################################
data_behav_addictions_flow <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                       "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq", "time_days")]
data_behav_addictions_flow_melt <- melt(data_behav_addictions_flow, id="time_days", variable="BA")
data_behav_addictions_flow_melt <- subset(data_behav_addictions_flow_melt, value %in% c("too much", "quite a lot"))
levels(data_behav_addictions_flow_melt$BA) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating", "BAs_other_freq" = "Other")
# data_behav_addictions_flow_melt$BA <- fct_relevel(data_behav_addictions_flow_melt$BA, 
#                                                "Other", "Gaming","Overeating","Gambling", "Legal substance(s)", "Smoking",
#                                                "Alcohol", "Illegal substance(s)", "Shopping")
# plot
BAs_tooMuch <- ggplot(data_behav_addictions_flow_melt, aes(x=time_days, fill=BA)) +
  # geom_bar() +
  geom_bar(position="fill") +
  labs(title=expression(paste("Frequency of ", bold("too much"), " or ", bold("quite a lot"), " answers within BA category over time")), 
       x="Time (Days since the outbreak)", y="Proportion", fill="") + 
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7", "#999999")) +
  labs(x="Time", y="Proportion", fill="") +
  theme_pubr(legend="right")  
BAs_tooMuch

ggsave(plot=last_plot(), filename="Figures/BAs/BAs_tooMuch.png") 

#######################################################
## BA = "too much" over time  PROPORTIONAL to each other
#######################################################
# prepare data of BAs
data_behav_addictions_time <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "time_days")]
data_behav_addictions_time_melt <- melt(data_behav_addictions_time, id="time_days", variable="BA")
data_behav_addictions_time_melt <- subset(data_behav_addictions_time_melt, !is.na(value))
data_behav_addictions_time_melt$value <- ifelse(data_behav_addictions_time_melt$value %in% c("too much", "quite a lot"), 1, 0)

# calculate mean "too much" accross categories (time, BA)
data_behav_addictions_time_melt <- subset(data_behav_addictions_time_melt, !is.na(value)) %>% 
  group_by(time_days, BA) %>%
  dplyr::summarise(too_much_01 = mean(value, na.rm=T))

# recode BA categories
levels(data_behav_addictions_time_melt$BA) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating")

# prepare distress data
data_distress <- data_shoppingCovid19[, c("time_days", "CISS", "stress_outbreak")]
data_distress$CISS_01 <- (data_distress$CISS - min(data_distress$CISS)) / max(data_distress$CISS)
# prepare covid19 stress data
data_distress$stress_outbreak_01 <- (data_distress$stress_outbreak - min(data_distress$stress_outbreak)) / max(data_distress$stress_outbreak)

# plot
span <- 0.4

BAs_tooMuch_perTime <- ggplot() +
  geom_smooth(method = "loess", 
              data=data_distress, 
              aes(y=CISS_01, x=time_days, fill="DISTRESS"), 
              linetype = "dashed", color="darkblue", 
              span = span) +
  geom_smooth(method = "loess", 
              data=data_distress, 
              aes(y=stress_outbreak_01, x=time_days, fill="COVID19 STRESS"), 
              linetype="dashed", color="darkblue", 
              span = span) +
  geom_smooth(method = "loess", 
              data=data_behav_addictions_time_melt, 
              aes(x=time_days, y=too_much_01, color=BA),
              se=F, 
              span = span) +
  scale_color_manual(values=BA_colors8) +
  labs(x="Time (Days since the outbreak)",
       y=expression(paste("Proportion of", bold( " too much"), " and", bold( " quite a lot"), " answers")),
       color="", 
       title="How often did you engage in [name of the activity] in the past 7 days?") +
  scale_fill_manual(name = "", values = c("DISTRESS" = "grey30", "this" = "green")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_pubr(legend="right") 
BAs_tooMuch_perTime

# save
ggsave(plot=last_plot(), filename="Figures/BAs/BAs_tooMuch_perTime.png", 
       width=20, height=12, units="cm") 

