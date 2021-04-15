
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
data_shoppingCovid19 <- read_rds(path="../../Data/data_shoppingCovid19_withScales_factorsAdjusted.rds")
dim(data_shoppingCovid19)

#######################################################
## distress and shopping + SES 
#######################################################

ggplot(data=subset(data_shoppingCovid19, !is.na(SES_subj)), 
       aes(x=CISS, y=COSS)) +
  geom_point(aes(color=PSS), position = "jitter") +
  geom_smooth(color="black") +
  coord_cartesian(ylim = c(27, 150)) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  scale_y_continuous(breaks = seq(0, 150, by = 10)) +
  theme_pubr()



