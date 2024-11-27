remove(list=ls())

renv::restore()

library(tidyverse) 
library(ggplot2)
library(lme4)
library(lmerTest)

#upload data
library(readxl)
layingdate <- read_excel("C:/Users/leono/Desktop/APCE2024/trait_project/Individual_Trait_Data_Excel.xlsx", sheet = "Buzzard_Friesland" ) |>
  na.omit() |>
  filter(sex2 == "female") |>
  dplyr::group_by(year, adult_ID) |>
  dplyr::summarize(laying=mean(julian_laying_date),
                   n_obs=n())
View(layingdate)

#selected temperature mean for the breeding season (March&April)
temp_data <- read_excel("C:/Users/leono/Desktop/APCE2024/trait_project/Individual_Trait_Data_Excel.xlsx", sheet = "Temp_Hoogeveen" ) |>
  na.omit() |>
  filter((Mn == 3 & Dd >= 8) | (Mn == 4 & Dd <= 13)) |>
  dplyr::group_by(YYYY) |>
  dplyr::summarize(tmp_avg=mean(AvgTemp24Hr)) |>
  filter(YYYY >= 1996 & YYYY <= 2015) |>
  dplyr::rename(year = YYYY)

# laying date data
laying_data <- 
  dplyr::left_join(layingdate, temp_data,by="year")

