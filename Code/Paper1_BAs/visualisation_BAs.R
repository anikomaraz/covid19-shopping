# author: Aniko Maraz

#######################################################
## packages and version setup
#######################################################
# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "scales", "readxl",
              "psych",
              "RColorBrewer", "ggpubr", "tidyjson")

# load (and/or install) packages with feedback of success
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

# check attributes of DF
dim(data_shoppingCovid19)
table(data_shoppingCovid19$time_batch)


#######################################################
## read Covid-19 cases data (only this chunk is Eva's code)
#######################################################

## source Covid-19 cases data
## downloaded from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

Covid19_geo_distirbution <- read_xlsx("Data/COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx")
data_covidCases <- Covid19_geo_distirbution %>% 
  filter(countryterritoryCode == "USA") %>% 
  filter(dateRep > "2020-03-26") %>%
  filter(dateRep < "2020-10-03") 

# check if data were imported correctly
min(data_covidCases$dateRep)
max(data_covidCases$dateRep)

data_covidCases <- data_covidCases %>% 
 select(dateRep, countryterritoryCode, cases, deaths, popData2019, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)

# create a new time variable: new format of date 26.03.2020
library(lubridate)
date <- strptime(as.character(data_covidCases$dateRep), "%Y-%m-%d")
date <- format(date, "%d.%m.%Y")

data_covidCases <- data.frame(data_covidCases, date)
data_covidCases <- data_covidCases %>% 
  select(date, countryterritoryCode, cases, deaths, popData2019, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)

#create a new variable "days_passed" to calculate the days into the epidemic since the official outbreak on the 13.03.2020
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

# save data_covidCases
write.csv(data_covidCases, file ="Data/data_covidCases.csv")

#######################################################
## settings for plots 
#######################################################
BA_colors8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
span = 0.2

#######################################################
## BAs
#######################################################
# extract behavioral addictions only
data_behav_addictions <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq")]

# set right format for plotting
data_behav_addictions_gather <- gather(data_behav_addictions, BA)
data_behav_addictions_gather$BA <- as.factor(data_behav_addictions_gather$BA)

# make levels more reader-friendly
data_behav_addictions_gather$BA <- factor(data_behav_addictions_gather$BA, 
                                          levels=c("BAs_gambling", "BAs_alcohol", "BAs_gaming", "BAs_illegal_drug",
                                                   "BAs_legal_drug", "BAs_other_freq", "BAs_overeating", "BAs_shopping", "BAs_smoking"), 
                                          labels=c("Gambling", "Alcohol", "Gaming", "Illegal substance(s)","Legal substance(s)",
                                                   "Other", "Overeating", "Shopping", "Smoking"))

# re-order to conventional listing
data_behav_addictions_gather$BA <- fct_relevel(data_behav_addictions_gather$BA, 
                                               "Other", "Gaming", "Shopping","Overeating","Smoking", "Legal substance(s)", "Gambling",  
                                              "Illegal substance(s)", "Alcohol")
data_behav_addictions_gather$value <- as.factor(data_behav_addictions_gather$value)

# make value labels more reader-friendly
data_behav_addictions_gather$value <- factor(data_behav_addictions_gather$value, 
                                             levels=c("not at all", "a little bit","somewhat", "quite a lot", 
                                                                                          "too much"))

# exclude missing data
data_behav_addictions_gather <- data_behav_addictions_gather[!is.na(data_behav_addictions_gather$value), ]


# plot
ggplot(data_behav_addictions_gather, aes(x=BA, fill=value)) +
  geom_bar(stat="count", position = "fill") +
  scale_fill_brewer(type="seq", palette="Oranges", na.value="grey80") +
  coord_flip() +
  labs(title="How often do you engage in the following behaviours?", y="Proportion", x="", fill="") +
  scale_y_continuous(n.breaks = 10)  +
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

# scaling (centering) for comparability
data_distress$PSS_cent <- scale(data_distress$PSS, center=T, scale=T)
data_distress$stress_outbreak_cent <- scale(data_distress$stress_outbreak, center=T, scale=T)

## Covid19 events in US economy
# start: 13.03.2020   President Trump issues the Proclamation on Declaring a National Emergency Concerning the Novel Coronavirus Disease (COVID-19) Outbreak, declaring a national state of emergency.[
# 27/03 Trump’s stimulus package # President Trump signs a $2 trillion stimulus package into law 
# The bill provides a one time payment of  a $1,200 check for individuals making up to $75,000 per year or $2,400 for couples earning less than $150,000. It also includes loans to businesses, funds unemployment insurance, bails out airlines and cargo carriers, authorizes aid to states and defers taxes, among other things
event_stimulus_package_day <- 30 # 09.04.2020
event_stimulus_package_name <- "Stimulus Package of $1,200 received"

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


# plot excessive behaviours
plot_BAs <- 
ggplot() +
  geom_smooth(method = "loess", data=data_behav_addictions_mean_time_melt,
              aes(x=time_days, y=value,
                  group = variable, color = variable),
              size = 0.7, 
              span = span, se = F)+
  scale_color_manual(values = BA_colors8) +
  labs(x = "",
       y = "Frequency",
       color = "", group = "",
       title = "How often did you engage in [name of the activity] in the past \n7 days?") +
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
  labs(x="",
       y="Z-scores",
       title="Distress during the outbreak") +
  scale_fill_manual(name = "", values = c(BA_colors8, 
                                          "Distress (PSS)" = "black", 
                                         "Covid-19 stress (single item)" = "darkred")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  theme_pubr(legend="top")

plot_distress

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


#######################################################
## PLOT CORR BETWEEN DISTRESS/COVID19 STRESS AND BEHAV.ADDICTIONS
#######################################################
# grep data from statistics file
source("Code/Paper1_BAs/statistics_BAs.R")

# prep for plotting
data_corr_stressType_melt <- melt(data_corr_stressType, id=c("stress_type", "time_days"), 
                                  value.name="value", na.rm=T)
# apply Fisher's Z transformation
data_corr_stressType_melt$value <- fisherz(data_corr_stressType_melt$value)

# make labels readable
levels(data_corr_stressType_melt$variable) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                           "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                           "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating")

## plot1 correlations with PSS by BA type
ggplot() + 
  geom_smooth(data=data_corr_stressType_melt[data_corr_stressType_melt$stress_type == "PSS", ], 
              aes(x=time_days, y=value, 
                  color=variable), 
              se = T) +
  labs(x="Time (days passed since the outbreak)\n", 
       y="Correlation (Kendall's tau)\n between distress and frequency of behaviour\n averaged for each timepoint", 
       color = "", 
       title = "Distress (PSS)") +
  scale_color_manual(values=BA_colors8) +
  facet_wrap("variable", nrow=3) +
  theme_pubr()

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/corr_byBA_facets_pss.png", 
       width=18, height=16, units="cm") 


## plot2 correlations with covid-19 stress (="stress_outbreak") by BA type

ggplot() + 
  geom_smooth(data=data_corr_stressType_melt[data_corr_stressType_melt$stress_type == "stress_outbreak", ], 
              aes(x=time_days, y=value, 
                  color=variable)) +
  labs(x="Time (days passed since the outbreak)\n", 
       y="Correlation coefficient\n between Covid-19 stress and frequency of behaviour\n averaged for each timepoint", 
       color = "", 
       title = "Covid-19-related stress (1 item)") +
  scale_color_manual(values = BA_colors8) +
  facet_wrap("variable") +
  theme_pubr()

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/corr_byBA_facets_covid19.png", 
       width=18, height=16, units="cm") 

