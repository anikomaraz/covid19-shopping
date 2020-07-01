
#######################################################
## packages and version setup
#######################################################

# ensure version compatibility
# library(checkpoint)
# checkpoint("2020-04-15")

## set up packages
packages <- c("tools", "tidyverse", "reshape2", "scales", "RColorBrewer", 
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
levels(data_shoppingCovid19$SES_subj) <- tools::toTitleCase(levels(data_shoppingCovid19_online$SES_subj))
levels(data_shoppingCovid19$income_now)[levels(data_shoppingCovid19_online$income_now) == "above 100k"] <- "above \n100k"
levels(data_shoppingCovid19$income_before)[levels(data_shoppingCovid19_online$income_before) == "above 100k"] <- "above \n100k"

# exclude those who did not wish to disclose their income
table(data_shoppingCovid19$income_now, data_shoppingCovid19$income_before)

data_shoppingCovid19 <- subset(data_shoppingCovid19, income_before != "Does not want to tell")
data_shoppingCovid19 <- subset(data_shoppingCovid19, income_now != "Does not want to tell")
data_shoppingCovid19$income_now <- factor(data_shoppingCovid19$income_now)
data_shoppingCovid19$income_before <- factor(data_shoppingCovid19$income_before)


#######################################################
## settings for plots 
#######################################################
SES_colors <- c("#313695", "#4575b4", "#74add1", 
                "#009E73", 
                "#f46d43", "#d73027", "#a50026")
SES_colors_3cat <- c("#313695", "#009E73", "#a50026")
goods_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",  "#FF9966", "#999999")

# spend amount
spent_categories <- c("less than 50", "50-100", "100-250", "250-500", "500-1000", "over 1000", "")


#######################################################
## debts 
#######################################################
debt <- data_shoppingCovid19[, c("COSS", "BERGEN", "debt_amount", "credit_card_amount")]

debt_melt <- melt(debt, id=c("COSS", "BERGEN"), variable="debt_and_credit")
debt_melt$value <- as.factor(debt_melt$value)

# set labels
levels(debt_melt$value) <- c("100-250", "250-600", "4000-10,000", "600-1200", 
                             "No unpaid \ndebt/credit", "less than 100", "over 10,000")

debt_melt$value <- fct_relevel(debt_melt$value,
                               "No unpaid \ndebt/credit", "less than 100", "100-250", "250-600", 
                               "600-1200", "4000-10,000", "over 10,000")
levels(debt_melt$debt_and_credit) <- c("Debt (company)", "Credit card")

# labels for debt
debt_no <- sum(data_shoppingCovid19$debt_amount == "does not want to tell", na.rm=T)
debt_less_than_100 <- sum(data_shoppingCovid19$debt_amount == "less_than_100", na.rm = T)
debt_100_250 <- sum(data_shoppingCovid19$debt_amount == "100_250", na.rm = T)
debt_250_600 <- sum(data_shoppingCovid19$debt_amount == "250_600", na.rm = T)
debt_600_1200 <- sum(data_shoppingCovid19$debt_amount == "600_1200", na.rm = T)
debt_4000_10000 <- sum(data_shoppingCovid19$debt_amount == "4000_10000", na.rm = T)
debt_over_10000 <- sum(data_shoppingCovid19$debt_amount == "over_10000", na.rm = T)

# assign label
levels(debt_melt$value) <- c(paste0("No unpaid \ndebt/credit", "\n", " N=", debt_no), 
                             paste0("less than 100", "\n", " N=", debt_less_than_100), 
                             paste0("100-250", "\n", " N=", debt_100_250), 
                             paste0("250-600", "\n", " N=", debt_250_600), 
                             paste0("600-1200", "\n", " N=", debt_600_1200), 
                             paste0("4000-10,000", "\n", " N=", debt_4000_10000), 
                             paste0("over 10,000", "\n", " N=", debt_over_10000))

# labels for credit
# create Ns
credit_no <- sum(data_shoppingCovid19$credit_card_amount == "does not want to tell", na.rm=T)
credit_less_than_100 <- sum(data_shoppingCovid19$credit_card_amount == "less_than_100", na.rm = T)
credit_100_250 <- sum(data_shoppingCovid19$credit_card_amount == "100_250", na.rm = T)
credit_250_600 <- sum(data_shoppingCovid19$credit_card_amount == "250_600", na.rm = T)
credit_600_1200 <- sum(data_shoppingCovid19$credit_card_amount == "600_1200", na.rm = T)
credit_4000_10000 <- sum(data_shoppingCovid19$credit_card_amount == "4000_10000", na.rm = T)
credit_over_10000 <- sum(data_shoppingCovid19$credit_card_amount == "over_10000", na.rm = T)

# assign label
levels(debt_melt$value) <- c(paste0("No unpaid \ndebt/credit", "\n", " N=", debt_no, "; ", credit_no), 
                             paste0("less than 100", "\n", " N=", debt_less_than_100, "; ", credit_less_than_100), 
                             paste0("100-250", "\n", " N=", debt_100_250, "; ", credit_100_250), 
                             paste0("250-600", "\n", " N=", debt_250_600, "; ", credit_250_600), 
                             paste0("600-1200", "\n", " N=", debt_600_1200, "; ", credit_600_1200), 
                             paste0("4000-10,000", "\n", " N=", debt_4000_10000, "; ", credit_4000_10000), 
                             paste0("over 10,000", "\n", " N=", debt_over_10000, "; ", credit_over_10000))

# ignore missing values
debt_melt <- subset(debt_melt, !is.na(value))

# plot online shopping
debtCredit_online <- ggplot(debt_melt,
       aes(x=value, y=COSS, fill=debt_and_credit)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  # display.brewer.all(colorblindFriendly=T)
  scale_fill_brewer(type="seq", palette="Dark2") +
  scale_color_brewer(type="seq", palette="Dark2") +
  labs(x="Amount (in USD)", y="Online Shopping", color="", fill="") +
  theme_pubr() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
debtCredit_online

# plot offline shopping
debtCredit_offline <- ggplot(debt_melt,
       aes(x=value, y=BERGEN, fill=debt_and_credit)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  # display.brewer.all(colorblindFriendly=T)
  scale_fill_brewer(type="seq", palette="Dark2") +
  scale_color_brewer(type="seq", palette="Dark2") +
  labs(x="Amount (in USD)", y="Offline Shopping", color="", fill="") +
  theme_pubr()
debtCredit_offline

# combine plots
ggarrange(debtCredit_online, debtCredit_offline, 
                                       ncol=1,
                                       common.legend = T)
# save
ggsave(last_plot(), filename="Figures/Income/debt.png", width=20, height=17, units="cm")

#######################################################
## compulsive buying + income AND compulsive buying +SES
#######################################################
# SES + shopping
data_shoppingCovid19_ses <- data_shoppingCovid19

# calculate N
SES_poorest <- sum(data_shoppingCovid19_ses$SES_subj == "Poorest", na.rm = T)
SES_poorer <- sum(data_shoppingCovid19_ses$SES_subj == "Poorer", na.rm = T)
SES_poor <- sum(data_shoppingCovid19_ses$SES_subj == "Poor", na.rm = T)
SES_average <- sum(data_shoppingCovid19_ses$SES_subj == "Average", na.rm = T)
SES_rich <- sum(data_shoppingCovid19_ses$SES_subj == "Rich", na.rm = T)
SES_richer <- sum(data_shoppingCovid19_ses$SES_subj == "Richer", na.rm = T)
SES_richest <- sum(data_shoppingCovid19_ses$SES_subj == "Richest", na.rm = T)

# assign labels
levels(data_shoppingCovid19_ses$SES_subj) <- 
  c(paste0("Poorest", "\n", " N=", SES_poorest), 
  paste0("Poorer", "\n", " N=", SES_poorer), 
  paste0("Poor", "\n", " N=", SES_poor), 
  paste0("Average", "\n", " N=", SES_average), 
  paste0("Rich", "\n", " N=", SES_rich), 
  paste0("Richer", "\n", " N=", SES_richer), 
  paste0("Richest", "\n", " N=", SES_richest))

# plot SES and online shopping
plot_ses_online <- ggplot(subset(data_shoppingCovid19_ses, !is.na(SES_subj)), 
                        aes(x=SES_subj, y=COSS, fill=SES_subj)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.3, fill="white") + 
  stat_boxplot(geom="errorbar", width=0.5) +
  labs(fill="",
       y="Online Shopping",
       title="Socio-economical status") +
  # # display.brewer.all(colorblindFriendly=T)
  scale_fill_brewer(type="seq", palette="BuPu", na.value="grey80") +
  theme_pubr(legend="none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
plot_ses_online

# plot SES and offline shopping
plot_ses_offline <- ggplot(subset(data_shoppingCovid19_ses, !is.na(SES_subj)), 
                          aes(x=SES_subj, y=BERGEN, fill=SES_subj)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.3, fill="white") + 
  stat_boxplot(geom="errorbar", width=0.5) +
  labs(fill="", 
       x="How wealthy do you think you are compared to others?", 
       y="Offline shopping") +
  # display.brewer.all(colorblindFriendly=T)
  scale_fill_brewer(type="seq", palette="BuPu", na.value="grey80") +
  theme_pubr(legend="none")
plot_ses_offline


# income + shopping
# prep data
data_income <- data_shoppingCovid19[, c("income_before", "income_now", "COSS", "BERGEN")]
data_income_melt <- melt(data_income, id=c("COSS", "BERGEN"), variable="income_type")
data_income_melt$value <- as.factor(data_income_melt$value)

# adjust labelling
data_income_melt$value <- fct_relevel(data_income_melt$value,
                                "under 15k", "15-25k", "25-35k", "35-50k", "50-75k",
                               "75-100k", "above \n100k")
levels(data_income_melt$income_type) <- c("Before the outbreak", "Past 30 days")


# calculate N
income_now_under_15k <- sum(data_income$income_now == "under 15k", na.rm = T)
income_now_15_25k <- sum(data_income$income_now == "15-25k", na.rm = T)
income_now_25_35k <- sum(data_income$income_now == "25-35k", na.rm = T)
income_now_35_50k <- sum(data_income$income_now == "35-50k", na.rm = T)
income_now_50_75k <- sum(data_income$income_now == "50-75k", na.rm = T)
income_now_75_100k <- sum(data_income$income_now == "75-100k", na.rm = T)
income_now_above_100k <- sum(data_income$income_now == "above \n100k", na.rm = T)

income_before_under_15k <- sum(data_income$income_before == "under 15k", na.rm = T)
income_before_15_25k <- sum(data_income$income_before == "15-25k", na.rm = T)
income_before_25_35k <- sum(data_income$income_before == "25-35k", na.rm = T)
income_before_35_50k <- sum(data_income$income_before == "35-50k", na.rm = T)
income_before_50_75k <- sum(data_income$income_before == "50-75k", na.rm = T)
income_before_75_100k <- sum(data_income$income_before == "75-100k", na.rm = T)
income_before_above_100k <- sum(data_income$income_before == "above \n100k", na.rm = T)

# assign labels
income_before_categories <- c(income_before_under_15k, income_before_15_25k, income_before_25_35k, 
                              income_before_35_50k, income_before_50_75k, income_before_75_100k, income_before_above_100k)
income_now_categories <- c(income_now_under_15k, income_now_15_25k, income_now_25_35k, 
                           income_now_35_50k, income_now_50_75k, income_now_75_100k, income_now_above_100k)

# for (i in income_before_categories) {
#  for (j in income_now_categories) {
#     levels(data_income_melt["value2", ]) <- c(paste0(levels(data_income_melt["value2", ]), "N=", i, ";", j)) 
#     print(levels(data_income_melt["value2", ]))
#    }
# }

levels(data_income_melt$value) <- 
    c(paste0("under 15k", "\n", " N=", income_before_under_15k, "; ", income_now_under_15k), 
    paste0("15-25k", "\n", " N=", income_before_15_25k, "; ", income_now_15_25k), 
    paste0("25-35k", "\n", " N=", income_before_25_35k, "; ", income_now_25_35k), 
    paste0("35-50k", "\n", " N=", income_before_35_50k, "; ", income_now_35_50k), 
    paste0("50-75k", "\n", " N=", income_before_50_75k, "; ", income_now_50_75k), 
    paste0("75-100k", "\n", " N=", income_before_75_100k, "; ", income_now_75_100k), 
    paste0("above \n100k", "\n", " N=", income_before_above_100k, "; ", income_now_above_100k))


# plot offline
plot_income_offline <- ggplot(data_income_melt, aes(x=value, y=BERGEN, fill=income_type)) +
  stat_boxplot(geom="errorbar", width=0.5, position=position_dodge(0.7)) +
  geom_boxplot(width=0.6, position = position_dodge(0.7)) + 
  # note: lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)
  # The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.
  labs(fill="", x="", y="Offline shopping", 
       title = "") +
  scale_fill_brewer(type="seq", palette="Purples") +
  theme_pubr(legend="none") 
plot_income_offline

# plot online
plot_income_online <- ggplot(data_income_melt, aes(x=value, y=COSS, fill=income_type)) +
  stat_boxplot(geom="errorbar", width=0.5, position=position_dodge(0.7)) +
  geom_boxplot(width=0.6, position = position_dodge(0.7)) + 
  # note: lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)
  # The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.
  labs(fill="", x="(in USD, per year)", y="Online shopping", 
       title = "Income") +
  scale_fill_brewer(type="seq", palette="Purples") +
  theme_pubr(legend="top") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
plot_income_online

# combine plots
incomeSES_shop <- ggarrange(plot_ses_online, plot_income_online, 
                            plot_ses_offline, plot_income_offline, 
                                 ncol=2, nrow=2)
incomeSES_shop

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/incomeSES_shop.png", 
       height = 12, width= 25, units="cm")


# #######################################################
# ## CB, stress and facet by SES  ON THE SAME PLOT
# #######################################################
# stress_cb_facetSES <- ggplot(subset(data_shoppingCovid19_online, !is.na(SES_subj)), aes(x=COSS, y=CISS)) +
#   geom_point() +
#   geom_smooth(stat="smooth", color="red") +
#   facet_wrap(~ SES_subj) +
#   labs(x="Shopping scale", y="Distress (CISS)", color="Legend text") +
#   theme_pubr()
# 
# stress_cb_facetSES
# 
# # save combined plot
# ggsave(plot=last_plot(), filename="Figures/Income/stress_cb_facetSES.png")


#######################################################
## CB, stress and facet by SES  ON THE SAME PLOT
#######################################################
span = 0.4

stress_cb_facetSES_smooth_online <- ggplot(subset(data_shoppingCovid19, !is.na(SES_subj)), aes(x=CISS, y=COSS)) +
  # geom_point(aes(color=SES_subj), position = position_jitterdodge()) +
  geom_smooth(stat="smooth", color="black", 
              aes(fill="Total"), 
              span = span)+
  geom_smooth(aes(group=SES_subj, color=SES_subj), 
              stat="smooth", se=F) +
  scale_color_manual(values=SES_colors)+
  labs(x="Distress", y="Online Shopping", color="Socio-economical \nstatus") +
  scale_fill_manual(name = "", values = c("Total" = "black")) +
  scale_y_continuous(breaks = seq(from=30, to=130, by=10)) +
  scale_x_continuous(breaks = seq(from=35, to=110, by=10)) +
  theme_pubr()

stress_cb_facetSES_smooth_online

# offline
stress_cb_facetSES_smooth_offline <- ggplot(subset(data_shoppingCovid19, !is.na(SES_subj)), aes(x=CISS, y=BERGEN)) +
  # geom_point(aes(color=SES_subj), position = position_jitterdodge()) +
  geom_smooth(stat="smooth", color="black", 
              aes(fill="Total"), 
              span = span) +
  geom_smooth(aes(group=SES_subj, color=SES_subj), stat="smooth", se=F) +
  # display.brewer.all(colorblindFriendly=T)
  scale_color_manual(values=SES_colors)+
  labs(x="Distress", y="Offline Shopping", color="Socio-economical \nstatus") +
  scale_fill_manual(name = "", values = c("Total" = "black")) +
  scale_y_continuous(breaks = seq(from=30, to=130, by=10)) +
  scale_x_continuous(breaks = seq(from=35, to=110, by=10)) +
  theme_pubr()

stress_cb_facetSES_smooth_offline


# save combined plot
ggarrange(stress_cb_facetSES_smooth_online, stress_cb_facetSES_smooth_offline, 
          ncol=2, 
          common.legend = T, legend="top")


ggsave(plot=last_plot(), filename="Figures/Income/stress_cb_facetSES_smooth.png", 
       width=20, height=15, units="cm")



#######################################################
## spent category over time
#######################################################

#create the mean of spending (value) at each point of assessment (time batch)
data_spend_category_names <- c("grocery_value", "clothes_value", "shoes_value", "jewellery_value", "electronics_value", 
                               "books_movies_music_games_value", "health_beauty_value",
                               "bags_accessories_value", "hobby_value", "gift_value", 
                               "time_days")
data_spend_categories <- aggregate(. ~ time_days, data_shoppingCovid19_online[, data_spend_category_names], 
                                   function(x) mean(x, na.rm=T), na.action=na.pass)

# prep data for ggplot
data_spend_categories_melt <- melt(data_spend_categories, id="time_days", variable="spend_category")
levels(data_spend_categories_melt$spend_category) <- c("Grocery", "Clothes", "Shoes", "Jewellery", "Electronics", "Books, Movies, \nMusic and Games", 
                                                       "Health and Beauty", "Bags and Accessories", "Hobby", "Gift")

# PLOT
span = 0.5

spend_raw <- ggplot(data=data_spend_categories_melt, 
                    aes(x=time_days, y=value, color=spend_category, group=spend_category)) +
  # geom_line(size=1.2) +
  geom_smooth(se=F, size=1.3, 
              span = span) +
  scale_color_manual(values=goods_colors) +
  # display.brewer.all(colorblindFriendly=T)
  labs(color="", x="Time (Days since the outbreak)", y="Spent Category ($)") +
  scale_y_continuous(breaks = 1:7, labels = spent_categories) +
  scale_x_continuous(breaks = seq(0, max(data_spend_category_time_ses_melt$time_days), by=5)) +
  theme_pubr(legend = "right")
spend_raw

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/spend_raw.png", 
       width=20, height=12, units="cm")


#######################################################
## spent category over time and SES
#######################################################

#create the mean of spending (value) at each point of assessment (time batch)
data_spend_category_time_ses <- c("grocery_value", "clothes_value", "shoes_value", "jewellery_value", "electronics_value",
                               "books_movies_music_games_value", "health_beauty_value",
                               "bags_accessories_value", "hobby_value", "gift_value",
                               "time_days", "SES_subj")
data_spend_category_time_ses <- data_shoppingCovid19[, data_spend_category_time_ses]
data_spend_category_time_ses <- subset(data_spend_category_time_ses, !is.na(SES_subj))

# prep data for ggplot
data_spend_category_time_ses_melt <- melt(data_spend_category_time_ses, 
                                        id=c("time_days", "SES_subj"))
levels(data_spend_category_time_ses_melt$variable) <- c("Grocery", "Clothes", "Shoes", "Jewellery", "Electronics", "Books, Movies, Music and Games", 
                                                       "Health and Beauty", "Bags and Accessories", "Hobby", "Gift")
data_spend_category_time_ses_melt$value <- as.factor(data_spend_category_time_ses_melt$value)

# order factor levels
data_spend_category_time_ses_melt$value <- fct_relevel(data_spend_category_time_ses_melt$value,
                                      "less_than_50", "50_100", "100_250", "250_500", "500_1000",
                                      "over_1000", "")

# transform them into integer, where 
#  "less_than_50" = 1, "50_100" = 2, "100_250" = 3, "250_500" = 4, "500_1000" = 4, "over_1000" = 5
data_spend_category_time_ses_melt$value <- as.integer(data_spend_category_time_ses_melt$value)

# get rid of value == 7, which means missing data or "do not want to tell"
data_spend_category_time_ses_melt <- subset(data_spend_category_time_ses_melt, !value == 7)


# calculate mean spending by keeping other categories (time, SES and spend_category)
data_spend_category_time_ses_melt <- aggregate(value ~ ., 
                                                    data_spend_category_time_ses_melt, mean)
  
# recode SES into only 3 categories
data_spend_category_time_ses_melt$SES_subj_3cat <- data_spend_category_time_ses_melt$SES_subj
data_spend_category_time_ses_melt$SES_subj_3cat <- dplyr::recode(data_spend_category_time_ses_melt$SES_subj_3cat, 
                                                          "Poorest" = "Poorest, Poorer or Poor", 
                                                          "Poorer" = "Poorest, Poorer or Poor", 
                                                          "Poor" = "Poorest, Poorer or Poor", 
                                                          "Average" = "Average",
                                                          "Richest" = "Richest, Richer or Rich", 
                                                          "Richer" = "Richest, Richer or Rich", 
                                                          "Rich" ="Richest, Richer or Rich")

# drop categories with too low N
data_spend_category_time_ses_melt <- data_spend_category_time_ses_melt[data_spend_category_time_ses_melt$variable %in% 
                                                           c("Grocery", "Clothes", "Electronics", "Books, Movies, \nMusic and Games", 
                                                             "Health and Beauty", "Hobby", "Gift"), ]

# PLOT and facet on spend category
spend_over_time_spend <-
  ggplot(data=data_spend_category_time_ses_melt,
                              aes(x=time_days, y=value, group=SES_subj_3cat, color=SES_subj_3cat)) +
  # geom_line() +
  geom_smooth(se=T, aes(fill=SES_subj_3cat), 
              span = 0.5) +
  # scale_color_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
  scale_color_manual(values=SES_colors_3cat) +
  scale_fill_manual(values=SES_colors_3cat) +
  facet_wrap(~variable, ncol=2) +
  labs(x="Time (Days since the outbreak)", y="Spending category ($)", color="", fill="") +
  guides(fill=FALSE) +
  ylim(0, 4.5) +
  scale_y_continuous(breaks = 1:7, labels = spent_categories) +
  theme_pubr()
spend_over_time_spend

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/spend_over_time_spend.png")

#######################################################
## spent category over time and SES  - CATEGORIES MERGED
#######################################################

# PLOT and facet on SES
spend_over_time_ses_catMerged <-
  ggplot(data=data_spend_category_time_ses_melt, 
         aes(x=time_days, y=value, group=variable, color=variable)) +
  # geom_line(stat="identity") +
  geom_smooth(span = 0.4, se=F) +
  scale_color_manual(values=goods_colors) +
  facet_wrap(~SES_subj_3cat, ncol=1) +
  labs(x="Time (Days since the outbreak)", y="Spending category ($)", color="") +
  scale_y_continuous(breaks = 1:7, labels = spent_categories) +
  scale_x_continuous(breaks = seq(0, max(data_spend_category_time_ses_melt$time_days), by=5)) +
  theme_pubr()
spend_over_time_ses_catMerged

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/spend_over_time_ses_catMerged.png")


#######################################################
## compulsive buying over time
#######################################################
data_cb_over_time <- data_shoppingCovid19[, c("SES_subj", "COSS", "BERGEN", "time_days")]
data_cb_over_time <- subset(data_cb_over_time, !is.na(SES_subj))

data_cb_over_time$SES_subj <- dplyr::recode(data_cb_over_time$SES_subj, 
                                                                 "Poorest" = "Poorest, Poorer or Poor", 
                                                                 "Poorer" = "Poorest, Poorer or Poor", 
                                                                 "Poor" = "Poorest, Poorer or Poor", 
                                                                 "Average" = "Average",
                                                                 "Richest" = "Richest, Richer or Rich", 
                                                                 "Richer" = "Richest, Richer or Rich", 
                                                                 "Rich" ="Richest, Richer or Rich")
data_cb_over_time_melt <- melt(data_cb_over_time, id=c("SES_subj", "time_days"), variable="shop_type")

# set span for plotting
span = 0.4

# plot online shopping
cb_over_time_online <- 
ggplot(subset(data_cb_over_time_melt, shop_type == "COSS"), aes(x=time_days, y=value)) + 
  geom_smooth(se=T, aes(fill="Total"), 
              linetype="dashed", 
              color="black", 
              span = span) + 
  geom_smooth(se=T, aes(color=SES_subj), 
              size=1.5, alpha=0.2, 
              span = span) +
  scale_x_continuous(breaks = seq(0, max(data_spend_category_time_ses_melt$time_days), by=5)) +
  labs(x="Time (Days since the outbreak)", y="Online shopping", 
       color="", group="", fill="") +
  scale_fill_manual(name = "", values = c(SES_colors_3cat, "Total" = "black")) +
  scale_color_manual(values=SES_colors_3cat) +
  scale_y_continuous(breaks = seq(20, 120, by=10)) +
  theme_pubr() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# plot offline shopping
cb_over_time_offline <- 
ggplot(subset(data_cb_over_time_melt, shop_type == "BERGEN"), 
       aes(x=time_days, y=value)) + 
  geom_smooth(se=T, aes(fill="Total"), 
              linetype="dashed", 
              color="black", 
              span = span) + 
  geom_smooth(se=T, aes(color=SES_subj), 
              size=1.5, alpha=0.2, 
              span= span) +
  scale_x_continuous(breaks = seq(0, max(data_spend_category_time_ses_melt$time_days), by=5)) +
  scale_fill_manual(name = "", values = c(SES_colors_3cat, "Total" = "black")) +
  labs(x="Time (Days since the outbreak)", y="Offline shopping", 
       color="", fill="") +
  scale_color_manual(values=SES_colors_3cat) +
  scale_y_continuous(breaks = seq(20, 120, by=10)) +
  theme_pubr()

# compbine plot
ggarrange(cb_over_time_online, cb_over_time_offline, 
          nrow = 2, common.legend = T)

# save plot
ggsave(plot=last_plot(), filename="Figures/Income/cb_over_time.png", 
       width=12, height=12, units = "cm")
