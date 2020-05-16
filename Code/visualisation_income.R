
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

# subset for online shopping only
data_shoppingCovid19_online <- subset(data_shoppingCovid19, online_purchase == 2)

# adjust labels

#######################################################
## compulsive buying + income AND compulsive buying +SES
#######################################################

# SES + shopping
plot_ses_shop <- ggplot(subset(data_shoppingCovid19_online, !is.na(SES_subj)), aes(x=SES_subj, y=COSS, fill=SES_subj)) +
  stat_boxplot(geom ="errorbar", width=0.4) +
  geom_boxplot(stat="boxplot") +
  labs(fill="", 
       x="How wealthy do you think you are compared to others?", 
       y="Shopping score", 
       title="Socio-economical status") +
  # display.brewer.all(colorblindFriendly=T)
  scale_fill_brewer(type="seq", palette="OrRd", na.value="grey80") +
  theme_pubr(legend="none")
plot_ses_shop

# income + shopping
# prep data
data_income <- data_shoppingCovid19_online[, c("income_now", "income_before", "COSS")]
data_income_melt <- melt(data_income, id="COSS")

# adjust labelling
data_income_melt$value <- as.factor(data_income_melt$value)
levels(data_income_melt$value)[levels(data_income_melt$value) == "above 100k"] <- "above \n100k"
levels(data_income_melt$value)[levels(data_income_melt$value) == "Does not want to tell"] <- "Does not \nwant to tell"

# plot
plot_income_shop <- ggplot(subset(data_income_melt, !is.na(value)), aes(x=value, y=COSS, fill=variable)) +
  # stat_boxplot(geom ="errorbar", width=0.4, position="dodge") +
  stat_boxplot(geom="errorbar") +
  stat_boxplot(geom="boxplot") +
  # note: lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)
  # The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.
  labs(fill="", x="(in US dollars, per month)", y="Shopping score", 
       title = "Income") +
  scale_fill_brewer(type="seq", palette="Purples") +
  theme_pubr(legend="top")
plot_income_shop


# combine & save plot
incomeSES_shop <- ggarrange(plot_ses_shop, plot_income_shop, 
                                 ncol=1, nrow=2)
incomeSES_shop

ggsave(plot=last_plot(), filename="Figures/Income/incomeSES_shop.png", 
       height = 25, width= 20, units="cm")



#######################################################
## compulsive buying and income+SES
#######################################################
# adjust labels
levels(data_shoppingCovid19$income_now)[levels(data_shoppingCovid19$income_now) == "above 100k"] <- "above \n100k"
levels(data_shoppingCovid19$income_now)[levels(data_shoppingCovid19$income_now) == "Does not want to tell"] <- "Does not \nwant to tell"

# SES, income and shopping
income_ses_CB <- ggplot(data=subset(data_shoppingCovid19, !is.na(SES_subj) & !is.na(income_now))) +
  geom_point(aes(x=SES_subj, y=income_now, colour = COSS), 
             position= "jitter") +
  scale_colour_gradient(low= "#88CCEE", high= "#661100") +
  labs(title="",
       y="Income (past month in US dollars)", 
       x="Socio-economical status (subjective)", 
       color="Shopping score") +
  theme_pubr(legend="right")
income_ses_CB

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/income_ses_CB.png", 
       units="cm", width=30, height=20) 


#######################################################
## CB, stress and facet by SES  ON THE SAME PLOT
#######################################################
stress_cb_facetSES <- ggplot(subset(data_shoppingCovid19, !is.na(SES_subj)), aes(x=COSS, y=CISS)) +
  geom_point() +
  geom_smooth(stat="smooth", color="red") +
  facet_wrap(~ SES_subj) +
  labs(x="Shopping scale", y="Distress (CISS)", color="Legend text") +
  theme_pubr()

stress_cb_facetSES

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Explorative/stress_cb_facetSES.png")



#######################################################
## CB, stress and facet by SES  ON THE SAME PLOT
#######################################################
stress_cb_facetSES_smooth <- ggplot(subset(data_shoppingCovid19, !is.na(SES_subj)), aes(x=COSS, y=CISS)) +
  geom_point(aes(color=SES_subj)) +
  geom_smooth(stat="smooth", color="black")+
  geom_smooth(aes(group=SES_subj, color=SES_subj), stat="smooth", se=F) +
  # display.brewer.all(colorblindFriendly=T)
  scale_color_manual(values=c("#313695", "#4575b4", "#74add1", "#6a0dad", "#f46d43", "#d73027", "#a50026"))+
  labs(x="Shopping scale", y="Distress (CISS)", color="Socio-economical \nstatus") +
  ylim(25, 100) +
  theme_pubr(legend="right")

stress_cb_facetSES_smooth

# save combined plot
ggsave(plot=last_plot(), filename="Figures/Income/stress_cb_facetSES_smooth.png", 
       width=20, height=15, units="cm")



