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


####### INITIAL VISUALIZATION ##########

# clutch size per year
visual_laying <- laying_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(laying_avg=mean(laying),
                   n_obs=n(),
                   laying_sd=sd(laying),
                   laying_se=laying_sd/sqrt(n_obs))
visual_laying <- 
  dplyr::left_join(visual_laying, temp_data,by="year")


#point plot laying date per year
laying_plot <- ggplot(visual_laying, aes(x= year, y=laying_avg)) + 
  geom_errorbar(aes(ymin=laying_avg-laying_se, ymax=laying_avg+laying_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE)
laying_plot

temp_plot <- ggplot(all_data, aes(x=year, y=tmp_avg)) + 
  geom_line()
temp_plot

# combine the figures
library(patchwork)
all_lay_plots<-laying_plot+temp_plot+
  patchwork::plot_layout(ncol=1) +
  patchwork::plot_annotation(title="Relations in model 1")
all_lay_plots


#point plot laying date with temperature
relation_plot <- ggplot(visual_laying, aes(x= tmp_avg, y=laying_avg)) + 
  geom_errorbar(aes(ymin=laying_avg-laying_se, ymax=laying_avg+laying_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE)
relation_plot

