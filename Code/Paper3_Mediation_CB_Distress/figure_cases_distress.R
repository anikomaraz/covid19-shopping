########Paper 3 figure for timeline########

library(tidyverse)
library(readxl)
setwd("D:/git/shopping_covid19")

## source Covid-19 cases data
## downloaded from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

Covid19_geo_distribution <- read_excel("Data/COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx")

data_covidCases <- Covid19_geo_distribution %>% 
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

######### prep distress/CovidStress data
data_shoppingCovid19 <- read_rds(path="../../Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
data_distress <- data_shoppingCovid19[, c("stress_outbreak", "time_days")]

# scaling (centering) for comparability
data_distress$stress_outbreak_cent <- scale(data_distress$stress_outbreak, center=T, scale=T)

plot_distress <- 
  ggplot(data_distress, aes(x=time_days, y=stress_outbreak)) +
  geom_smooth(color="black") +
  labs(title="COVID-related distress", 
       x= "Time (days since the outbreak)\n between 26/03/2020 and 02/10/2020",
       y= "stress")


## Covid19 events in US economy
# start: 13.03.2020   President Trump issues the Proclamation on Declaring a National Emergency Concerning the Novel Coronavirus Disease (COVID-19) Outbreak, declaring a national state of emergency.[
# 27/03 Trump’s stimulus package # President Trump signs a $2 trillion stimulus package into law 
# The bill provides a one time payment of  a $1,200 check for individuals making up to $75,000 per year or $2,400 for couples earning less than $150,000. It also includes loans to businesses, funds unemployment insurance, bails out airlines and cargo carriers, authorizes aid to states and defers taxes, among other things
# event_stimulus_package_day <- 30 # 09.04.2020
# event_stimulus_package_name <- "Stimulus Package of $1,200 received"
# 
# # stay-at-home orders lifted # between 26 April and 13 May
# event_lockdown_lifted_day <- 44 # 26.04.2020
# event_lockdown_lifted_name <- "Lockdown lifting begins"
# 
# # The George Floyd protests begin in Minneapolis
# event_GeorgeFloyd_day <-  74 # 26.05.2020
# event_GeorgeFloyd_name <- "George Floyd protests begin"
# 
# # The House Appropriations Committee approved a measure requiring masks on public transportation
# event_mask_day <- 123 # 14.07.2020
# event_mask_name <- "Masks on public transport"
# 
# # July 28: The CDC calls for reopening American schools
# event_schools_day <-  137 # 28.07.2020
# event_schools_name <- "CDS calls for reopening\n  schools"


############### plot Covid19 cases
plot_cases <- 
  ggplot() +
  # add Covid cases
  geom_smooth(data = data_covidCases, aes(y = cases, x = days_passed), 
              color = "black", method = "loess", se=F) +
  labs(title="New Covid-19 cases", 
       x= "Time (days since the outbreak)\n between 26/03/2020 and 02/10/2020") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  scale_y_continuous(n.breaks = 8)

# +
#   # add Covid19 events
#   annotate(geom="text", x=event_stimulus_package_day + 20, y=50000, label=event_stimulus_package_name,
#            color="red") +
#   annotate(geom="text", x=event_lockdown_lifted_day + 21, y=40000, label=event_lockdown_lifted_name,
#            color="red") +
#   annotate(geom="text", x=event_mask_day + 15, y=73000, label=event_mask_name,
#            color="red") +
#   annotate(geom="text", x=event_GeorgeFloyd_day + 15, y=30000, label=event_GeorgeFloyd_name,
#            color="red") +
#   annotate(geom="text", x=event_schools_day + 32, y=63000, label=event_schools_name,
#            color="red") +
#   
#   # add arrows
#   geom_segment(aes(x = event_stimulus_package_day, y = 48000, 
#                    xend = event_stimulus_package_day + 0.5, yend = 43000),
#                arrow = arrow(length = unit(0.3, "cm")), color="red") + 
#   geom_segment(aes(x = event_lockdown_lifted_day, y = 38000, 
#                    xend = event_lockdown_lifted_day + 0.5, yend = 33000),
#                arrow = arrow(length = unit(0.3, "cm")), color="red") + 
#   geom_segment(aes(x = event_mask_day, y = 71000, 
#                    xend = event_mask_day + 0.5, yend = 66000),
#                arrow = arrow(length = unit(0.3, "cm")), color="red") +
#   geom_segment(aes(x = event_GeorgeFloyd_day, y = 28000, 
#                    xend = event_GeorgeFloyd_day + 0.5, yend = 23000),
#                arrow = arrow(length = unit(0.3, "cm")), color="red") +
#   geom_segment(aes(x = event_schools_day, y = 66000, 
#                    xend = event_schools_day + 0.5, yend = 61000),
#                arrow = arrow(length = unit(0.3, "cm")), color="red") +
#   theme_pubr(legend="right")


######################################to-do!!!##################################
#1) need to insert T1/T2/T3 on plot!
####2) push to git
######3) add to paper as png



# merge plot with facets
ggarrange(plot_cases, plot_distress, ncol=1, nrow=3, 
          common.legend = F, align = "v")


# save plot
ggsave(plot=last_plot(), filename="Code/Paper3_Mediation_CB_Distress/plot_cases_distress.png", 
       width=18, height=27, units="cm") 


