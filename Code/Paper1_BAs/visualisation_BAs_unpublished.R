# author: Aniko Maraz


#######################################################
## packages and version setup
#######################################################
# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tidyverse", "reshape2", "scales", "RColorBrewer", 
              # "cowplot", 
              "psych",
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

# check basic dimensions
dim(data_shoppingCovid19)
table(data_shoppingCovid19$time_batch)

#######################################################
## source corr data from published plots file  (for Covid-19 cases and correlations)
#######################################################
source(file ="Code/Paper1_BAs/visualisation_BAs.R")


#######################################################
## settings for plots 
#######################################################
BA_colors8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
span = 0.2


#######################################################
## plot correlations with PSS by BA type
#######################################################

# plot
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
ggsave(plot=last_plot(), filename="Figures/BAs/not_published/corr_byBA_facets_covid19.png", 
       width=18, height=16, units="cm") 



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

# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/not_published/BAs_tooMuch.png") 

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
ggsave(plot=last_plot(), filename="Figures/BAs/not_published/BAs_tooMuch_perTime.png", 
       width=20, height=12, units="cm") 

