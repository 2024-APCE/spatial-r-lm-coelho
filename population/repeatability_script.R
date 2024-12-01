remove(list=ls())


renv::restore()

library(tidyverse) 
library(ggplot2)
library(lme4)
library(lmerTest)

#upload data
CS1 <- read.table("https://docs.google.com/spreadsheets/d/e/2PACX-1vQVcphvEPyp-VCzTdRQUtE_M4KkylBLS9yfzNGSCAtC08NrOhupLAZPMeQgxd4TLIK11JPhq9cMqV5p/pub?gid=1736259690&single=true&output=csv", h=T, sep=",") |>
  na.omit(CS) |>
  as_tibble()

#check for outliers, for example:
hist(CS1$CS)



#### step 1. linear regression ###
m1<-lm(CS~annual_density, data=CS1)
summary(m1)

# Visualizing regression:
ggplot(CS1, aes(x=annual_density, y=CS)) +geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)

# check residuals
par(mfrow=c(2,2))
plot(m1)
hist(residuals(m1))



#### step 2. linear regression - density as factor ###
CS1$annual_density_cat <- as.factor(CS1$annual_density_cat)
m2 <- lm(CS~annual_density_cat, data=CS1)
summary(m2)
#Estimate, is the first value of the category
# In the smaller densitity there is 10.45 - 0.19 (Sd) eggs 
#Small data increases degrees of freedom lowering the power of significance



#### step 3. linear regressio - taking annual means
CS2 <- CS1 %>% 
  dplyr::group_by(annual_density) %>%
  dplyr::summarize(CS_avg=mean(CS),
                   CS_sd=sd(CS),
                   n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))

m3<-lm(CS_avg~annual_density, data=CS2)
summary(m3)
# here we can see for a lower data the error increases
# residual error is smaller because observations are closer to the mean

#visualisation
ggplot(CS2, aes(x=annual_density, y=CS_avg)) + 
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_point()+
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)
  
  
###EXERCISE CLUTCH SIZE

names(CS1)
lmer()
m1<-glmer(CS ~ annual_density + (1|plotID)+ (1|femaleID), data= CS1, family="gaussian")

summary(m1)
confint(m1)
#years here can be selecetd has
#factor, to see the differences between years
#continuous, to see trend in clutch trhout the yeas
#random, if th conditions were simialar between years


m1<-lmer(CS ~ + (1|femaleID), data=CS1)
summary(m1)
confint(m1)

#Repetability can explain trends of the results (genetics)???
#Repetability R (IC) = VarianceIntercept/VarianceIntercept +...+ VarianceResiduals
r <- 1.211/(1.211+1.544)
r

##############################################

install.packages("squid")
install.packages("shiny")
install.packages("markdown")

library(squid)
library(shiny)
library(markdown)
squidApp()

#### RANDOM INTERCEPT MODEL
# high VI, large variance of individuals -> high r
# high Vm, high slope variance (it seemn diference responses but is actually measure error)

#### RANDOM SLOPE MODEL

#bulding a model with no intercept-slope covariance
Model <-lmer(CS ~ annual_density +(1|femaleID)+(0+ annual_density|femaleID), data=CS1)
summary(Model)



