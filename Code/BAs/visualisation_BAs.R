
#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "scales", "RColorBrewer", 
              # "cowplot", 
              "ggpubr")

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
data_distress15 <- data_shoppingCovid19[, c("CISS", "stress_outbreak", "time_days")]
data_distress15$CISS_15 <- (data_distress15$CISS - min(data_distress15$CISS)) / max(data_distress15$CISS) * 5 +2
data_distress15$stress_outbreak_15 <- (data_distress15$stress_outbreak - min(data_distress15$stress_outbreak)) / max(data_distress15$stress_outbreak) * 5 +2

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



# plot excessive behaviours
plot_BAs <- 
ggplot() +
  geom_smooth(method = "loess", data=data_behav_addictions_mean_time_melt,
              aes(x=time_days, y=value,
                  group = variable, color = variable),
              size = 0.7, 
              span=span, se=F)+
  scale_color_manual(values=BA_colors8) +
  labs(x="Time (days since the outbreak)\n between 26/03/2020 and 02/10/2020",
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
  geom_smooth(method= "loess", data=data_distress15, 
              aes(y=CISS_15, x=time_days, fill="DISTRESS"), 
              linetype = "dashed", color="darkblue", 
              span = span) +
  geom_smooth(method = "loess", data=data_distress15, 
              aes(y=stress_outbreak_15, x=time_days, fill="COVID19 STRESS"), 
              linetype="dashed", color="darkblue", 
              span = span) +
  
  # scale_fill_manual(values=BA_colors8) +
  scale_color_manual(values=BA_colors8) +
  labs(x="",
       y="",
       color="", group = "",
       title="Distress during the outbreak") +
  scale_fill_manual(name = "", values = c(BA_colors8, "DISTRESS" = "grey15", "COVID19 STRESS" = "purple")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  theme_pubr(legend="top")

# plot Covid19 cases
plot_cases <- 
  ggplot() +
    # add Covid cases
  geom_smooth(data = data_covidCases, aes(y = cases, x = days_passed), 
              color = "black", method = "loess", se=F) +
  labs(title="New Covid-19 cases") +
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
# EVA
# calculate Spearman correlation at each point of time between distress (PSS scale) and each behavioral addiction (variables starting with BAs_  but leave out the BAs_other) 
plotcorrdisandBA<- data.frame(as.numeric(data_shoppingCovid19$BAs_shopping), as.numeric(data_shoppingCovid19$BAs_alcohol), 
                         as.numeric(data_shoppingCovid19$BAs_smoking), as.numeric(data_shoppingCovid19$BAs_legal_drug),
                         as.numeric(data_shoppingCovid19$BAs_illegal_drug), as.numeric(data_shoppingCovid19$BAs_gambling),
                         as.numeric(data_shoppingCovid19$BAs_gaming), as.numeric(data_shoppingCovid19$BAs_overeating), data_shoppingCovid19$PSS, data_shoppingCovid19$stress_outbreak)
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
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.PSS"] <- "PSS"
names(plotcorrdisandBA)[names(plotcorrdisandBA) == "data_shoppingCovid19.stress_outbreak"] <- "stress_outbreak"


#Correlations between each behavioural problem and PSS
ashopping<-cor.test(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
aalcohol<-cor.test(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
asmoking<-cor.test(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
alegaldrugs<-cor.test(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
aillegaldrugs<-cor.test(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
agambling<-cor.test(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
agaming<-cor.test(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$PSS, method="spearman", na.rm = T)
aovereating<-cor.test(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$PSS, method="spearman", na.rm = T) 

a<-c(ashopping$estimate, aalcohol$estimate, asmoking$estimate, alegaldrugs$estimate, aillegaldrugs$estimate, agambling$estimate, agaming$estimate, aovereating$estimate)

#separately: Covid-19 stress (stress_outbreak variable) and each behavioural addiction. In total 8 behavioural addictions, meaning 16 correlations at each point of time. 
cor.test(plotcorrdisandBA$BAs_shopping, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_alcohol, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_smoking, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_legal_drug, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_illegal_drug, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gambling, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_gaming, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)
cor.test(plotcorrdisandBA$BAs_overeating, plotcorrdisandBA$stress_outbreak, method="kendall", na.rm = T)



#create two plots, separately for distress, and for covid19 stress. The strength 
#of the correlation should be on the y axis (mean + SE or SD calculated for each assessment), 
#and time (days_passed) on the x axis, and of course a separate line for each behavioural addiction.
plot(data_shoppingCovid19$time_days, a)


# IGNORED PLOTS


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

