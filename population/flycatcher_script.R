### Repeatability analysis
## import datasets
library(readxl)
PF_Traits_OneperYear_Young <- read_excel("C:/Users/leono/Desktop/APCE2024/flycatcher_data/PF_Traits_OneperYear_Young.xlsx")
View(PF_Traits_OneperYear_Young) #use this one

PF_Traits_OneperYear_Adult2024 <- read_excel("C:/Users/leono/Desktop/APCE2024/flycatcher_data/PF_Traits_OneperYear_Adult2024.xlsx")
View(PF_Traits_OneperYear_Adult2024) # use this one

PF_AvgTraits_perIndividual2024 <- read_excel("C:/Users/leono/Desktop/APCE2024/flycatcher_data/PF_AvgTraits_perIndividual2024.xlsx")
View(PF_AvgTraits_perIndividual2024)

Pedigree_PF2024 <- read_excel("C:/Users/leono/Desktop/APCE2024/flycatcher_data/Pedigree_PF2024.xlsx")
View(Pedigree_PF2024)

Hoogeveen_Daily <- read_excel("C:/Users/leono/Desktop/APCE2024/flycatcher_data/Hoogeveen_Daily.xlsx")
View(Hoogeveen_Daily)

## install packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lme4)

## Repeatability analysis
# Convert Mass to numeric if it is not already
PF_Traits_OneperYear_Adult2024$Mass <- as.numeric(as.character(PF_Traits_OneperYear_Adult2024$Mass))
# Convert Tarsus to numeric if it is not already
PF_Traits_OneperYear_Adult2024$Tarsus <- as.numeric(as.character(PF_Traits_OneperYear_Adult2024$Tarsus))
# check if there are any na values in mass
sum(is.na(PF_Traits_OneperYear_Adult2024$Mass))

#model of mass and with individual ID as a random effect and remove any na values from the mass data set
flycatcherdata<- PF_Traits_OneperYear_Adult2024 |>
  select(Individual, Sex, Mass) |>
  na.omit()

m1<-lmer(Mass ~ (1|Individual), data=flycatcherdata)
confint(m1)
summary(m1)

## Repeatability (r) = Vi /(Vi +... +Vr)
repeatability = (1.527^11)/(1.527^11 + 3.499^25)
repeatability

## what is the sample size of individuals of the dataset
n = length(unique(flycatcherdata$Individual))
n

# same model but with also year as random effect
m2<-lmer(Mass ~ (1|Individual) + (1|Year), data=PF_Traits_OneperYear_Adult2024)
confint(m2)
summary(m2)
## Repeatability (r) = Vi /(Vi +Vr)
repeatability = (0)/(8.518^22 + 3.499^25)
repeatability



# calculate heritability
library(ggplot2)
# create new data frame linking children to parents
heritability <- read_excel("Flycatcher_Data2024 (2)/Query1.xlsx")
# plot model to find heritability
ggplot(heritability, aes(x=mean, y=Child)) +
  geom_point(shape=1) +# Use hollow circles
  geom_smooth(method=lm, se=FALSE)

herit<-lm(Child~mean, data=heritability)
summary(herit)
# heritability = 0.179
# sample size of heritability dataset
n2 = length(unique(heritability$Child))
n2


##############################################

#calculate repeatability of clutch size
traitdata<- PF_Traits_OneperYear_Adult2024 |>
  select(Year, Individual, Mass, ClutchSize, Recruits) |>
  na.omit()

m_trait<-lmer(ClutchSize ~ (1|Individual), data=traitdata)
confint(m_trait)
summary(m_trait)

r <- (0.1011)/(0.1011 + 0.3938)
r


# calculate heritability
library(ggplot2)
# create new data frame linking children to parents
heritabi <- read_excel("Flycatcher_Data2024 (2)/Query1.xlsx")
# plot model to find heritability
ggplot(heritability, aes(x=mean, y=Child)) +
  geom_point(shape=1) +# Use hollow circles
  geom_smooth(method=lm, se=FALSE)

herit<-lm(Child~mean, data=heritability)
summary(herit)
# heritability = 0.179
# sample size of heritability dataset
n2 = length(unique(heritability$Child))
n2



#calculate selection for a trait
avg_clutch <- mean(traitdata$ClutchSize)
avg_clutch 

sd_clutch <- sd(traitdata$ClutchSize)
sd_clutch

recruit_data <- traitdata |>
  filter(Recruits !=0)
wo_clutch <-mean(recruit_data$ClutchSize)
wo_clutch 

trait <- (wo_clutch-avg_clutch)/sd_clutch
trait





