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
              se=FALSE) +
  labs(title = "Laying date from 1996 to 2015",
       x = "Year",
       y = "Average laying date") + 
  scale_x_continuous(breaks = seq(1995, 2015, by = 5)) +
  theme(text = element_text(size=18),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.3, "cm"))
laying_plot

temp_plot <- ggplot(all_data, aes(x=year, y=tmp_avg)) + 
  geom_line() +
  labs(title = "Temperature variation from 1996 to 2015",
       x = "Year",
       y = "Average temperature") + 
  scale_x_continuous(breaks = seq(1995, 2015, by = 5)) +
  theme(text = element_text(size=18),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.3, "cm"))
temp_plot

# clutch size variation per year
clutch_data <- buzzard_data |>
  filter(sex2 == "female") |>
  dplyr::group_by(year) |>
  dplyr::summarize(clutch_avg=mean(eggs),
                   n_obs=n(),
                   clutch_sd=sd(eggs),
                   clutch_se=clutch_sd/sqrt(n_obs))
clutch_data <-
  dplyr::left_join(clutch_data, temp_data,by="year")

#point plot clutch size per year
clutch_plot <- ggplot(clutch_data, aes(x=year, y=clutch_avg)) + 
  geom_errorbar(aes(ymin=clutch_avg-clutch_se, ymax=clutch_avg+clutch_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE) +
  labs(title = "Clutch size from 1996 to 2015",
       x = "Year",
       y = "Average clutch size") + 
  scale_x_continuous(breaks = seq(1995, 2015, by = 5)) +
  theme(text = element_text(size=18),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.3, "cm"))
clutch_plot


# combine the figures
library(patchwork)
all_plots<- clutch_plot+laying_plot+temp_plot+
  patchwork::plot_layout(ncol=1)
all_plots




# combine the figures
library(patchwork)
all_lay_plots<-laying_plot+temp_plot+
  patchwork::plot_layout(ncol=1) +
  patchwork::plot_annotation(title="Relations in model 1")
all_lay_plots

##########################################################

########## MODELS ##################

# 1- model relation between clutch size and random factors (adultID and year??)
# No fixed effects are specified, meaning this is a random intercepts-only model
lay_m1 <-lmerTest::lmer(laying ~ + (1|adult_ID) + (1|year), data=laying_data)
summary(lay_m1)
confint(lay_m1)
# sd reflects the variability within the data points
# low sd
# WHAT TO TAKE FROM THIS?!

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r1_lay_adultID <- 52.08/(52.08+44.86)
r1_lay_adultID # 0.54
# year
r1_lay_year <- 20.06/(20.06+44.86)
r1_lay_year # 0.31



# 2- model of clutchsize and temperature with ramdom factors 
lay_m2 <- lmerTest::lmer(laying ~ tmp_avg + (1|adult_ID) + (1|year), data=laying_data)
summary(lay_m2)
confint(lay_m2)

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r2_lay_adultID <- 51.52/(51.52+45.00)
r2_lay_adultID # 0.53
# year
r2_lay_year <- 13.03/(13.03+45.00)
r2_lay_year # 0.22



# 3- model of PLASTICITY
#Model with (within individual effect) and  (between individual effect)
#Data with centered temperature per individual
plast_lay_data <- laying_data |>
  dplyr::group_by(adult_ID) |>
  dplyr::summarize(btw_lay_affect=mean(tmp_avg)) #between individual temperature ->avg temperature per female

count(plast_lay_data)

## Between individual effect: mean temperature for each female! This is how individuals differ
laying_data <- laying_data |> left_join(plast_lay_data, by = "adult_ID")

## Within individual effect: how each value differs from individual 
laying_data <- laying_data |> 
  dplyr::mutate(within_lay_affect = tmp_avg - btw_lay_affect)


#Model with (within individual effect) and  (between individual effect)
lay_m3 <-lmerTest::lmer(laying ~ within_lay_affect + btw_lay_affect + (1|adult_ID),
                               data= laying_data)
summary(lay_m3)
confint(lay_m3) ## do not overlap w 0 but overlap with each other so both within and between effect explain variation

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r3_lay_adultID <- 42.48/(42.48+56.11)
r3_lay_adultID # 0.43



####### PLOT #######
#point plot laying date with temperature
relation_plot <- ggplot(visual_laying, aes(x= tmp_avg, y=laying_avg)) + 
  geom_errorbar(aes(ymin=laying_avg-laying_se, ymax=laying_avg+laying_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE) +
  labs(title = "Variation of laying date with temperature",
       x = "Average temperature",
       y = "Average laying date") + 
  theme(text = element_text(size=20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.3, "cm"))
relation_plot

