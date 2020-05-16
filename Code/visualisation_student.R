
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
ggsave(plot=last_plot(), filename="Figures/Students/students_income.png")



#######################################################
## age and student
#######################################################
age_online <- ggplot(data_shoppingCovid19, aes(x=age, y=COSS)) +
  geom_smooth(aes(group=study, color=study, fill=study), alpha=0.3) +
  geom_point(position="jitter", aes(color=study)) +
  scale_color_manual(values=c("#009E73", "#D55E00")) +
  scale_fill_manual(values=c("#009E73", "#D55E00")) +
  labs(x="", y="Online shopping", color="", fill="") +
  theme_bw()
age_online

age_offline <- ggplot(data_shoppingCovid19, aes(x=age, y=BERGEN)) +
  geom_smooth(aes(group=study, color=study, fill=study), alpha=0.3) +
  geom_point(position="jitter", aes(color=study)) +
  scale_color_manual(values=c("#009E73", "#D55E00")) +
  scale_fill_manual(values=c("#009E73", "#D55E00")) +
  labs(x="Age", y="Offline shopping", color="", fill="") +
  theme_bw()
age_offline

age_compulsive_buying <- ggarrange(age_online, age_offline,
                                   common.legend = T, legend="top",
                                   ncol=1, nrow=2)
# age_compulsive_buying <- annotate_figure(age_compulsive_buying, fig.lab = "Socio-economical status",
#                                fig.lab.size=15, fig.lab.face = "bold", fig.lab.pos = "top.right")
age_compulsive_buying

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/age_compulsive_buying.png")


