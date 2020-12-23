
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





#######################################################
## settings for plots 
#######################################################
BA_colors8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# ,  "#FF9966"
# , "#999999"

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
# 27/03 Trump’s stimulus package # President Trump signs a $2 trillion stimulus package into law 
# The bill provides a one time payment of  a $1,200 check for individuals making up to $75,000 per year or $2,400 for couples earning less than $150,000. It also includes loans to businesses, funds unemployment insurance, bails out airlines and cargo carriers, authorizes aid to states and defers taxes, among other things
event_stimulus_package_day <- 29 # 09.04.2020
event_stimulus_package_name <- "Stimulus Package of $1,200"

# stay-at-home orders lifted # between 26 April and 13 May
event_lockdown_lifted_day <- 46 # 26.04.2020
event_lockdown_lifted_name <- "Lockdown lifting"

# The House Appropriations Committee approved a measure requiring masks on public transportation
event_mask_day <- 125 # 14.07.2020
event_mask_name <- "Masks on public transport"



# plot
span = 0.2

ggplot() +
  geom_smooth(method = "loess", data=data_behav_addictions_mean_time_melt,
              aes(x=time_days, y=value,
                  group = variable, color = variable),
              span=span, se=F)+
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
  labs(x="Time (days since the outbreak)\n between 26/03/2020 and 02/10/2020",
       y=expression(paste("Frequency,", "   ", "where 1 = Not at all", "  ", "5 = Too much")),
       color="", group = "",
       title="How often did you engage in [name of the activity] in the past 7 days?") +
  scale_fill_manual(name = "", values = c(BA_colors8, "DISTRESS" = "grey30", "COVID19 STRESS" = "purple")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  # add Covid19 events
  annotate(geom="text", x=event_stimulus_package_day + 20, y=7, label=event_stimulus_package_name,
           color="red") +
  annotate(geom="text", x=event_lockdown_lifted_day + 15, y=6, label=event_lockdown_lifted_name,
           color="red") +
  annotate(geom="text", x=event_mask_day + 15, y=6.4, label=event_mask_name,
           color="red") +
  # add arrows
  geom_segment(aes(x = event_stimulus_package_day, y = 6.8, 
                   xend = event_stimulus_package_day + 0.5, yend = 6),
                   arrow = arrow(length = unit(0.3, "cm")), color="red") + 
  geom_segment(aes(x = event_lockdown_lifted_day, y = 5.8, 
                   xend = event_lockdown_lifted_day + 0.5, yend = 5),
               arrow = arrow(length = unit(0.3, "cm")), color="red") + 
  geom_segment(aes(x = event_mask_day, y = 6.2, 
                   xend = event_mask_day + 0.5, yend = 5.4),
               arrow = arrow(length = unit(0.3, "cm")), color="red") +
  theme_pubr(legend="right")

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/BAs_all_perTime.png", 
       width=20, height=12, units="cm") 





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

