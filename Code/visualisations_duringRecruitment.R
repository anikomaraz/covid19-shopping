
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
data_shoppingCovid19 <- read_rds(path="Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
dim(data_shoppingCovid19)

#######################################################
## compulsive buying + income AND compulsive buying +SES
#######################################################
# income + offline (Bergen) shopping
violin_income_bergen <- ggplot(data_shoppingCovid19, aes(x=income_now, y=BERGEN, fill=income_now)) +
  geom_violin() +
  labs(title="Offline shopping") +
  theme_bw()
violin_income_bergen

# SES + offline (Bergen) shopping
violin_ses_bergen <- ggplot(data_shoppingCovid19, aes(x=SES_subj, y=BERGEN, fill=SES_subj)) +
  geom_violin() +
  labs(title="Offline shopping") +
  theme_bw()
violin_ses_bergen

# income + online (COSS) shopping
violin_income_coss <- ggplot(data_shoppingCovid19, aes(x=income_now, y=COSS, fill=income_now)) +
  geom_violin() +
  labs(title="Online shopping") +
  theme_bw()
violin_income_coss

# SES + online (COSS) shopping
violin_ses_coss <- ggplot(data_shoppingCovid19, aes(x=SES_subj, y=COSS, fill=SES_subj)) +
  geom_violin() +
  labs(title="Online shopping") +
  theme_bw()
violin_ses_coss

# combine & save plot
violin_CB_incomeSES <- ggarrange(violin_ses_bergen, violin_ses_coss, 
                                 violin_income_bergen, violin_income_coss, 
                                 ncol=2, nrow=2)
violin_CB_incomeSES

ggsave(plot=last_plot(), filename="Figures/Explorative/violin_ses_cos.png", 
    units="cm", width=30) 

#######################################################
## compulsive buying and income+SES
#######################################################
# for offline shopping (Bergen scale)
OfflineCB_income_ses <- ggplot(data=data_shoppingCovid19) +
  geom_point(aes(x=income_now, y=SES_subj, colour = BERGEN), 
             position= "jitter") +
  scale_colour_gradient(low= "#88CCEE", high= "#661100") +
  labs(title="Offline shopping") +
  theme_bw()

# for online shopping (COSS)
OnlineCB_income_ses <- ggplot(data=data_shoppingCovid19) +
  geom_point(aes(x=income_now, y=SES_subj, colour = COSS), 
             position= "jitter") +
  scale_colour_gradient(low= "#88CCEE", high= "#661100") +
  labs(title="Online shopping") +
  theme_bw()

# combine plots
CB_incomeSES <- ggarrange(OfflineCB_income_ses, OnlineCB_income_ses, 
                          ncol=1, nrow=2)
CB_incomeSES

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/CB_incomeSES.png", 
       units="cm", width=20) 

#######################################################
## spent category over time
#######################################################
#set variables
data_shoppingCovid19$time_batch <- as.factor(data_shoppingCovid19$time_batch)
data_shoppingCovid19$grocery_value <- as.numeric(data_shoppingCovid19$grocery_value)

#create the mean of spending (value) at each point of assessment (time batch)
data_spend_category_names <- c("grocery_value", "clothes_value", "shoes_value", "jewellery_value", "electronics_value", 
                               "books_movies_music_games_value", "health_beauty_value",
                               "bags_accessories_value", "hobby_value", "gift_value", 
                               "time_batch")
data_spend_categories <- aggregate(. ~ time_batch, data_shoppingCovid19[, data_spend_category_names], 
                                   function(x) mean(x, na.rm=T), na.action=na.pass)

# prep data for ggplot
data_spend_categories_melt <- melt(data_spend_categories)

# PLOT
spend_raw <- ggplot(data=data_spend_categories_melt, aes(x=time_batch, y=value, color=variable, group=variable)) +
  geom_line() +
    theme_bw()
spend_raw

spend_proportional <- ggplot(data=data_spend_categories_melt, aes(x=time_batch, y=value, group=variable)) + 
    geom_area(position="fill", aes(fill=variable)) +
  geom_line(position="fill", size=0.5, color="black") +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",  "#FF9966", "#999999")) +
  theme_bw()
spend_proportional

# combine plots
spend_plot <- ggarrange(spend_raw, spend_proportional, ncol=1, nrow=2)

# save plot
ggsave(plot=last_plot(), filename="Figures/Explorative/spend_categories.png")


#######################################################
## CB and stress
#######################################################
stress_onlineCB <- ggplot(data_shoppingCovid19, aes(x=CISS, y=COSS)) +
  geom_point(aes(group=SES_subj, color=SES_subj), position="jitter") +
  geom_smooth(stat="smooth") +
  xlab("Compulsive buying online (COSS)") + ylab("Distress (CISS)") +
  labs(color="SES subjective") +
  scale_color_brewer(type="seq", palette="Purples", na.value="grey80") +
  theme_bw()

stress_offlineCB <- ggplot(data_shoppingCovid19, aes(x=CISS, y=BERGEN)) +
  geom_point(aes(group=SES_subj, color=SES_subj), position="jitter") +
  geom_smooth(stat="smooth") +
  xlab("Compulsive buying offline (Bergen)") + ylab("Distress (CISS)") +
  labs(color="SES subjective") +
  scale_color_brewer(type="seq", palette="Reds", na.value="grey80") +
  theme_bw()

# combine plots
stress_CB <- ggarrange(stress_onlineCB, stress_offlineCB, 
                       ncol=1, nrow=2)
stress_CB

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/stress_CB.png") 

#######################################################
## CB, stress and facet by SES  ON THE SAME PLOT
#######################################################
# library("grid")
# library("ggplot2")
# library("ggpubr")
cb_stress_ses_unite <- ggplot(subset(data_shoppingCovid19, !is.na(SES_subj)), aes(x=CISS)) +
  geom_point(aes(y=COSS), position="jitter", color="red") +
  geom_smooth(aes(y=COSS), stat="smooth", color="red") +
  geom_point(aes(y=BERGEN), position="jitter", color="blue") +
  geom_smooth(aes(y=BERGEN), stat="smooth", color="blue") +
  facet_wrap(~ SES_subj) +
  labs(x="Shopping scale (online or offline)", y="Distress (CISS)", color="Legend text") +
  theme_pubr()

cb_stress_ses_unite <- cb_stress_ses_unite + grid.text("Online shopping", x=0.75, y=0.2, 
                                gp=gpar(col="red"))
cb_stress_ses_unite + grid.text("Offline shopping", x=0.75, y=0.15, 
                                gp=gpar(col="blue"))
cb_stress_ses_unite

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/stress_CB_ses_unite.png")

#######################################################
## students
#######################################################
students_income_online <- ggplot(data_shoppingCovid19, 
                                 aes(x=SES_subj, y=COSS, fill=study)) +
  geom_violin() +
  scale_fill_manual(values=c("green", "red")) +
  # scale_color_brewer(type="seq", palette="Greens", na.value="grey80") +
  labs(x="", y="Online shopping", 
       fill=NULL) +
  theme_bw()

students_income_offline <- ggplot(data_shoppingCovid19, 
                          aes(x=SES_subj, y=BERGEN, fill=study)) +
  geom_violin() +
  scale_fill_manual(values=c("green", "red")) +
  # scale_color_brewer(type="seq", palette="Greens", na.value="grey80") +
  labs(x="", y="offline shopping", 
       fill=NULL) +
  theme_bw()

# combine plots
students_income <- ggarrange(students_income_online, students_income_offline,
                             common.legend = T,
                             ncol=1, nrow=2)
students_income <- annotate_figure(students_income, fig.lab = "Student status", 
                                   fig.lab.size=15, fig.lab.face = "bold")
students_income

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/students_income.png")

#######################################################
## works
#######################################################
work_income_online <- ggplot(data_shoppingCovid19, 
                                 aes(x=SES_subj, y=COSS, fill=work)) +
  geom_violin() +
  scale_fill_manual(values=c("#D55E00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(x="", y="Online shopping", 
       fill=NULL) +
  theme_bw()
work_income_online

work_income_offline <- ggplot(data_shoppingCovid19, 
                                  aes(x=SES_subj, y=BERGEN, fill=work)) +
  geom_violin() +
  scale_fill_manual(values=c("#D55E00", "#CC79A7", "#56B4E9", "#009E73")) +
  # scale_fill_brewer(na.value="grey80") +
  labs(x="", y="Offline shopping", 
       fill=NULL) +
  theme_bw()
work_income_offline

# combine plots
work_income <- ggarrange(work_income_online, work_income_offline,
                             common.legend = T,
                             ncol=1, nrow=2)
work_income <- annotate_figure(work_income, fig.lab = "Work status", 
                                   fig.lab.size=15, fig.lab.face = "bold")
work_income

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/work_income.png")


#######################################################
## age
#######################################################
age_online <- ggplot(data_shoppingCovid19, aes(x=age, y=COSS)) +
  geom_smooth() +
  geom_point(position="jitter", aes(color=SES_subj)) +
  scale_color_brewer(type="seq", palette="Oranges", na.value="grey80") +
  labs(x="", y="Online shopping", color="") +
  theme_bw()

age_offline <- ggplot(data_shoppingCovid19, aes(x=age, y=BERGEN)) +
  geom_smooth() +
  geom_point(position="jitter", aes(color=SES_subj)) +
  scale_color_brewer(type="seq", palette="Purples", na.value="grey80") +
  labs(x="Age", y="Offline shopping", color="") +
  theme_bw()
  
age_compulsive_buying <- ggarrange(age_online, age_offline,
                                   common.legend = F, legend="right",
                                   ncol=1, nrow=2)
# age_compulsive_buying <- annotate_figure(age_compulsive_buying, fig.lab = "Socio-economical status",
#                                fig.lab.size=15, fig.lab.face = "bold", fig.lab.pos = "top.right")
age_compulsive_buying

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/age_compulsive_buying.png")


#######################################################
## BAs
#######################################################
data_behav_addictions <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq")]

data_behav_addictions_gather <- gather(data_behav_addictions, BA)
data_behav_addictions_gather$BA <- as.factor(data_behav_addictions_gather$BA)
data_behav_addictions_gather$BA <- factor(data_behav_addictions_gather$BA, levels=c("BAs_other_freq", "BAs_overeating","BAs_gaming","BAs_gambling",
                                                                                    "BAs_illegal_drug","BAs_legal_drug","BAs_alcohol","BAs_smoking", "BAs_shopping"))
data_behav_addictions_gather$value <- as.factor(data_behav_addictions_gather$value)
data_behav_addictions_gather$value <- factor(data_behav_addictions_gather$value, levels=c("not at all", "a little bit","somewhat", "quite a lot", 
                                                                                    "too much"))
data_behav_addictions_gather <- data_behav_addictions_gather[!is.na(data_behav_addictions_gather$value), ]

ggplot(data_behav_addictions_gather, aes(x=BA, fill=value)) +
  geom_bar(stat="count", position = "fill") +
  scale_fill_brewer(type="seq", palette="Oranges", na.value="grey80") +
  coord_flip() +
  labs(title="How often do you engage in the following behaviours?", ylab="Response") +
  theme_bw()

# save plot
ggsave(plot=last_plot(), filename="Figures/Explorative/BAs.png", 
       units="cm", width=20) 


#######################################################
## BA = "too much" over time
#######################################################
data_behav_addictions_flow <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                  "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq", "time_batch")]
data_behav_addictions_flow_melt <- melt(data_behav_addictions_flow, id="time_batch", variable="BA")
data_behav_addictions_flow_melt <- subset(data_behav_addictions_flow_melt, value %in% c("too much", "quite a lot"))
levels(data_behav_addictions_flow_melt$BA) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                "BAs_legal_drug" = "Legal Drug", "BAs_illegal_drug" = "Illegal Substance", "BAs_gambling" = "Gambling", 
                                                "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating", "BAs_other_freq" = "Other")
table(data_behav_addictions_flow_melt$time_batch)

  # plot
BAs_tooMuch <- ggplot(data_behav_addictions_flow_melt, aes(x=time_batch, fill=BA)) +
  # geom_bar() +
  geom_bar(position="fill") +
  labs(title=expression(paste("Frequency of ", bold("too much"), " or ", bold("quite a lot"), " answers within BA category over time")), 
       x="Time", y="Proportion", fill="") + 
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7", "#999999")) +
  theme_bw()
BAs_tooMuch

# # plot2
# ggplot(data_behav_addictions_flow_melt, aes(x=time_batch, group=BA, color=BA)) +
#   geom_line(stat="count", position="fill", size=1.5) +
#   labs(title=expression(paste("Relative frequency of ", bold("too much"), " or ", bold("quite a lot"), " answers within BA category over time"))) +
#   theme_bw()

ggsave(plot=last_plot(), filename="Figures/Explorative/BAs_tooMuch.png") 

#######################################################
## gaming for Orsi
#######################################################
ggplot(subset(data_shoppingCovid19, !is.na(BAs_gaming)), aes(y=age, x=BAs_gaming, color=as.factor(gender))) +
  geom_point(position="jitter", na.rm=T) +
  xlab(expression(paste("How often did you engage in the following behaviours", bold(" in the past 7 days"), "?"))) + 
  ylab("Age") +
  labs(color="") +
  scale_color_manual(values=c("red", "blue", "green", "grey")) +
  theme_bw()

# save plot
ggsave(plot=last_plot(), filename="Figures/Gaming_Orsi/points.png", 
       units="cm", width=20) 


ggplot(subset(data_shoppingCovid19, !is.na(BAs_gaming)), aes(y=age, x=BAs_gaming, color=as.factor(gender))) +
  geom_violin(aes(fill=BAs_gaming)) +
  xlab("How often did you engage in gaming in the past 7 days?") + 
  ylab("Age") +
  labs(color="", fill="") +
  scale_color_manual(values=c("red", "blue", "green", "grey")) +
  theme_bw()

# save plot
ggsave(plot=last_plot(), filename="Figures/Gaming_Orsi/violins.png", 
       units="cm", width=20) 
