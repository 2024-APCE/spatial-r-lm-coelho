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

# clutch size per year per temp data
clutch_data <- buzzard_data |>
  filter(sex2 == "female") |>
  dplyr::group_by(year) |>
  dplyr::summarize(clutch_avg=mean(eggs),
                   n_obs=n(),
                   clutch_sd=sd(eggs),
                   clutch_se=clutch_sd/sqrt(n_obs))
clutch_data <-
  dplyr::left_join(clutch_data, temp_data,by="year")


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

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r1_adultID <- 0.097/(0.097+0.429)
r1_adultID # 0.18
# year
r1_year <- 0.056/(0.056+0.429)
r1_year # 0.12


############ point PLOT clutch size with temperature
clutch_tmp <- ggplot(clutch_data, aes(x= tmp_avg, y=clutch_avg)) + 
  geom_errorbar(aes(ymin=clutch_avg-clutch_se, ymax=clutch_avg+clutch_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line # BETTER SMOOTH?!
              se=FALSE) + 
  labs(title = "Variation of clutch size with temperature",
       x = "Average temperature",
       y = "Average clutch size") + 
  theme(text = element_text(size=20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.3, "cm"))
clutch_tmp
############


# 2- model of clutchsize and temperature with ramdom factors 
m_temp <- lmerTest::lmer(clutch_avg ~ tmp_avg + (1|adult_ID) + (1|year), data=m_data)
summary(m_temp)
confint(m_temp)

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r2_adultID <- 0.097/(0.097+0.429)
r2_adultID # 0.18
# year
r2_year <- 0.043/(0.043+0.429)
r2_year # 0.09




# 3- model of PLASTICITY
#Model with (within individual effect) and  (between individual effect)
#Data with centered temperature per individual
plast_data <- m_data |>
  dplyr::group_by(adult_ID) |>
  dplyr::summarize(btw_affect=mean(tmp_avg)) #between individual temperature ->avg temperature per female

count(plast_data)

## Between individual effect: mean temperature for each female! This is how individuals differ
m_data <- m_data %>% left_join(plast_data, by = "adult_ID")

## Within individual effect: how each value differs from individual 
m_data <- m_data |> 
  dplyr::mutate(within_affect = tmp_avg - btw_affect)


#Model with (within individual effect) and  (between individual effect)
m_individuals <-lmerTest::lmer(clutch_avg  ~within_affect + btw_affect + (1|adult_ID),
                     data= m_data)
summary(m_individuals)
confint(m_individuals) ## do not overlap w 0 but overlap with each other so both within and between effect explain variation

#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
# repeatability explains how much a variable explains the response variable
# adult_ID
r3_adultID <- 0.093/(0.093+0.468)
r3_adultID # 0.17



