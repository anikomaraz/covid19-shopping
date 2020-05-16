
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
                                               "Other", "Gaming","Overeating","Gambling", "Legal substance(s)", "Smoking",
                                               "Alcohol", "Illegal substance(s)", "Shopping")
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
  theme(legend.position="top") + theme_pubr()


# save plot
ggsave(plot=last_plot(), filename="Figures/BAs/BAs.png", 
       units="cm", width=20) 


#######################################################
## BA = "too much" over time
#######################################################
data_behav_addictions_flow <- data_shoppingCovid19[, c("BAs_shopping", "BAs_alcohol", "BAs_smoking", "BAs_legal_drug", "BAs_illegal_drug", 
                                                       "BAs_gambling", "BAs_gaming", "BAs_overeating", "BAs_other_freq", "time_batch")]
data_behav_addictions_flow_melt <- melt(data_behav_addictions_flow, id="time_batch", variable="BA")
data_behav_addictions_flow_melt <- subset(data_behav_addictions_flow_melt, value %in% c("too much", "quite a lot"))
levels(data_behav_addictions_flow_melt$BA) <- c("BAs_shopping" = "Shopping", "BAs_alcohol" = "Alcohol", "BAs_smoking" = "Smoking",
                                                "BAs_legal_drug" = "Legal Substance(s)", "BAs_illegal_drug" = "Illegal Substance(s)", "BAs_gambling" = "Gambling", 
                                                "BAs_gaming" = "Gaming", "BAs_overeating" = "Overeating", "BAs_other_freq" = "Other")
# data_behav_addictions_flow_melt$BA <- fct_relevel(data_behav_addictions_flow_melt$BA, 
#                                                "Other", "Gaming","Overeating","Gambling", "Legal substance(s)", "Smoking",
#                                                "Alcohol", "Illegal substance(s)", "Shopping")
# plot
BAs_tooMuch <- ggplot(data_behav_addictions_flow_melt, aes(x=time_batch, fill=BA)) +
  # geom_bar() +
  geom_bar(position="fill") +
  labs(title=expression(paste("Frequency of ", bold("too much"), " or ", bold("quite a lot"), " answers within BA category over time")), 
       x="Time", y="Proportion", fill="") + 
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7", "#999999")) +
  labs(x="Time", y="Proportion", fill="") +
  theme_pubr(legend="right")  
BAs_tooMuch

ggsave(plot=last_plot(), filename="Figures/BAs/BAs_tooMuch.png") 

# plot2
ggplot(data_behav_addictions_flow_melt, aes(x=time_batch, group=BA, color=BA)) +
  geom_area(stat="count", position="fill", 
            aes(fill=BA), alpha=0.5) +
            # aes(fill=fct_reorder(BA, value, .desc=T)), alpha=0.5) +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7", "#999999")) +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7", "#999999")) +
  labs(title=expression(paste("Relative frequency of ", bold("too much"), " or ", bold("quite a lot"), " answers within BA category over time"))) +
  labs(x="Time", y="Proportion", fill="", color="") +
  theme_pubr()

ggsave(plot=last_plot(), filename="Figures/BAs/BAs_tooMuch_area.png") 




