
renv::restore()

library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot
library(ggplot2)


# read the data from the google docs link:
SEMdata <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR-cr8z8Pye7gakP0ihX5SUtNLU8h_evJAySe4ZcnRL1d2HAoOkHKmDe9cM6uA4oC1A4QRArIlxFX5V/pub?gid=1240187235&single=true&output=csv")
names(SEMdata)


# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  



# make a pairs panel to inspect linearity of relations and expected normality of residuals
library(psych)
psych::pairs.panels(SEMdata %>% dplyr::select(dist2river,elevation,rainfall,valleys,burnfreq,
                                       CEC,PA,woody),
                    stars = T, ellipses = F)
#standarize just change the units (to negative) but the relation stays the same
psych::pairs.panels(SEMdatastd %>% dplyr::select(dist2river,elevation,rainfall,valleys, burnfreq, 
                                          CEC, PA, woody),
                    stars = T, ellipses = F, cex.labels = 1.5, cex.cor = 2)



# visualization of the result: 
#browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model
#model is describe in a string '...'
woody_model <- 'woody~dist2river+elevation+rainfall+valleys+burnfreq+CEC+PA
                rainfall~elevation
                valleys~dist2river
                burnfreq~dist2river+elevation
                CEC~dist2river+elevation+rainfall+burnfreq+PA'
woody_model
woody_fit <- lavaan::sem(woody_model, data=SEMdatastd)

# show the model results
summary(woody_fit, standardized=T, fit.measures=T, rsquare=T)
# CFI=0.785 and TLI=0.403 -> NOT GOOD MODEL!!
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# this is not the best model to explain woody cover variation 
#there is the need to check causalities of some variables!

#make another model Piecewise!!!
#(e.g. where Rainfall don't affect woody directly but only CEC/ PA only influencing CEC/ maybe taking valleys out?!)


## Selected variables in order to get a better model
woody_model2 <- 'woody~burnfreq+CEC+valleys
                rainfall~elevation
                valleys~dist2river
                burnfreq~dist2river
                CEC~rainfall+burnfreq+PA'
woody_model2
woody_fit2 <- lavaan::sem(woody_model2, data=SEMdatastd)

# show the model results
summary(woody_fit2, standardized=T, fit.measures=T, rsquare=T)
# CFI=0.778 and TLI=0.653 -> BETTER BUT STILL NOT A GOOD MODEL...
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR



## Selected RAINFALL AND DISTANCE TO RIVER in order to get a better model
woody_model3 <- 'woody~burnfreq+CEC+valleys
                rainfall~elevation+dist2river
                valleys~dist2river
                burnfreq~dist2river
                CEC~rainfall+burnfreq+PA'
woody_model3
woody_fit3 <- lavaan::sem(woody_model3, data=SEMdatastd)

# show the model results
summary(woody_fit3, standardized=T, fit.measures=T, rsquare=T)
# CFI=0.989 and TLI=0.982 -> GOOD MODEL!!!
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
