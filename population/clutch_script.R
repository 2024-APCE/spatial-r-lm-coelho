remove(list=ls())
install.packages("lmerTest")

renv::restore()

library(tidyverse) 
library(ggplot2)
library(lme4)
library(lmerTest)

#upload data
library(readxl)
buzzard_data <- read_excel("C:/Users/leono/Desktop/APCE2024/trait_project/Individual_Trait_Data_Excel.xlsx", sheet = "Buzzard_Friesland" ) |>
  na.omit()

View(buzzard_data)

# WHY MALES HAVE EGGS??! SHOULD I FILTER FEMALES
# Filtrate for March and April - breeding season?!!!

####### INITIAL VISUALIZATION ##########

# clutch size per year
clutch_data <- buzzard_data |>
  filter(sex2 == "female") |>
  dplyr::group_by(year) |>
  dplyr::summarize(clutch_avg=mean(eggs),
                   n_obs=n(),
                   clutch_sd=sd(eggs),
                   clutch_se=clutch_sd/sqrt(n_obs))


#bar plot clutch size per year
ggplot(clutch_data, aes(x=year, y=clutch_avg)) + 
  geom_bar(stat = "identity")

#point plot clutch size per year
clutch_plot <- ggplot(clutch_data, aes(x=year, y=clutch_avg)) + 
  geom_errorbar(aes(ymin=clutch_avg-clutch_se, ymax=clutch_avg+clutch_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE)
clutch_plot
  

#join temperature data to buzzard data
all_data <- 
  dplyr::left_join(clutch_data, temp_data,by="year")

temp_plot <- ggplot(all_data, aes(x=year, y=tmp_avg)) + 
  geom_line()
temp_plot

# combine the figures
library(patchwork)
allplots<-clutch_plot+temp_plot+
  patchwork::plot_layout(ncol=1) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

########### MODELS ############

#building data base for model analysis
model_data <- buzzard_data |>
  filter(sex2 == "female") |>
  dplyr::group_by(year, adult_ID) |>
  dplyr::summarize(clutch_avg=mean(eggs),
                   n_obs=n())
m_data <-
  dplyr::left_join(model_data, temp_data,by="year")


#1ºmodel relation between clutch size and random factors (adultID and year??)
# No fixed effects are specified, meaning this is a random intercepts-only model
m_random <-lmerTest::lmer(clutch_avg ~ + (1|adult_ID) + (1|year), data=m_data)
summary(m_random)
confint(m_random)
# sd reflects the variability within the data points
# low sd
# WHAT TO TAKE FROM THIS?!


#lineal model of clutch size and teperature
m1<-lm(clutch_avg~tmp_avg, data=m_data)
summary(m1)
# visualization
ggplot(m_data, aes(x=tmp_avg, y=clutch_avg)) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)


#model of clutchsize and temperature with ramdom factors 
m_temp <- lmerTest::lmer(clutch_avg ~ tmp_avg + (1|adult_ID) + (1|year), data=m_data)
summary(m_temp)
confint(m_temp)

model_plot <- ggplot(m_data, aes(x=year, y=clutch_avg)) +
  geom_point()+
  geom_smooth(method="loess")  # Add linear regression line # BETTER SMOOTH?!
model_plot

