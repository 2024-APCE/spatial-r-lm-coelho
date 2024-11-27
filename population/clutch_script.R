remove(list=ls())

renv::restore()

library(tidyverse) 
library(ggplot2)
library(lme4)

#upload data
library(readxl)
buzzard_data <- read_excel("C:/Users/leono/Desktop/APCE2024/trait_project/Individual_Trait_Data_Excel.xlsx", sheet = "Buzzard_Friesland" ) |>
  na.omit()

View(buzzard_data)

# WHY MALES HAVE EGGS??! SHOULD I FILTER FEMALES
# Filtrate for March and April - breeding season?!!!




# clutch size per year
clutch <- buzzard_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(clutch_avg=mean(eggs),
                   n_obs=n(),
                   clutch_sd=sd(eggs),
                   n_obs=n(),
                   clutch_se=clutch_sd/sqrt(n_obs))


#bar plot clutch size per year
ggplot(clutch, aes(x=year, y=clutch_avg)) + 
  geom_bar(stat = "identity")

#point plot clutch size per year
ggplot(clutch, aes(x=year, y=clutch_avg)) + 
  geom_errorbar(aes(ymin=clutch_avg-clutch_se, ymax=clutch_avg+clutch_se), width=.1) +
  geom_point()+
  geom_smooth(method=lm,   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE)
  
#join temperature data to buzzard data
temp_data <- read_excel("C:/Users/leono/Desktop/APCE2024/trait_project/Individual_Trait_Data_Excel.xlsx", sheet = "EnvironmentalCovariates" ) |>
  na.omit()

clutch_data <- 
  dplyr::left_join(clutch,,by="species_ID")


