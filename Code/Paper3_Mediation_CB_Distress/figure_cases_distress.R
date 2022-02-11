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
data_shoppingCovid19 <- read_rds(path="Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
data_distress <- data_shoppingCovid19[, c("stress_outbreak", "time_days")]

# scaling (centering) for comparability
data_distress$stress_outbreak_cent <- scale(data_distress$stress_outbreak, center=T, scale=T)

plot_distress <- 
  ggplot(data = data_distress, aes(x=time_days, y=stress_outbreak)) +
  geom_smooth(color="black") +
  labs(title="COVID-related distress", 
       x= "Time (days since the outbreak)\n between March 26, 2020 and October 2, 2020",
       y= "stress")+
  geom_rect(data = data_distress, aes(xmin = 14, xmax = 80, ymin = 5.8, ymax= 7.5), color = "#d6d698", fill = "#d6d698",
            alpha= 0.01) +
  geom_text(data = data_covidCases,
            aes(x = 50, y = 7.2, label = "T1")) +
  geom_rect(data = data_distress, aes(xmin = 81, xmax = 140, ymin = 5.8, ymax= 7.5), color = "#ADD8E6", fill = "#ADD8E6",
            alpha= 0.01) +
  geom_text(data = data_distress,
            aes(x = 110, y = 7.2, label = "T2")) +
  geom_rect(data = data_distress, aes(xmin = 141, xmax = 206, ymin = 5.8, ymax= 7.5), color = "#AFD878", fill = "#AFD878",
            alpha= 0.01) +
  geom_text(data = data_distress,
            aes(x = 175, y = 7.2, label = "T3"))+
  coord_cartesian(ylim = c(5.8, 7.5))


plot_distress


############### plot COVID-19 cases

plot_cases <- 
  ggplot() +
  geom_smooth(data = data_covidCases, aes(y = cases, x = days_passed), 
              color = "black", method = "loess", se=F) +
  labs(title="New Covid-19 cases", 
       x= "Time (days since the outbreak)\n between March 26, 2020 and October 2, 2020") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  scale_y_continuous(n.breaks = 8) +
  geom_rect(data = data_covidCases, aes(xmin = 14, xmax = 80, ymin = 0, ymax= 60000), color = "#d6d698", fill = "#d6d698",
            alpha= 0.01) +
  geom_text(data = data_covidCases,
            aes(x = 50, y = 5000, label = "T1")) +
  geom_rect(data = data_covidCases, aes(xmin = 81, xmax = 140, ymin = 0, ymax= 60000), color = "#ADD8E6", fill = "#ADD8E6",
            alpha= 0.01) +
  geom_text(data = data_covidCases,
            aes(x = 110, y = 5000, label = "T2")) +
  geom_rect(data = data_covidCases, aes(xmin = 141, xmax = 206, ymin = 0, ymax= 60000), color = "#AFD878", fill = "#AFD878",
            alpha= 0.01) +
  geom_text(data = data_covidCases,
            aes(x = 175, y = 5000, label = "T3"))


plot_cases



# merge plot with facets
ggarrange(plot_cases, plot_distress, ncol=1, nrow=3, heights = c(2,2),
          common.legend = F, align = "v")


# save plot
ggsave(plot=last_plot(), filename="Code/Paper3_Mediation_CB_Distress/plot_cases_distress.png", 
       width=18, height=35, units="cm") 


