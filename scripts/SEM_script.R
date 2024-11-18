
renv::restore()

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
psych::pairs.panels(SEMdata %>% select(dist2river,elevation,rainfall,valleys, burnfreq, 
                                       CEC, PA, woody),
                    stars = T, ellipses = F)
#standarize just change the units (to negative) but the relation stays the same
psych::pairs.panels(SEMdatastd %>% select(dist2river,elevation,rainfall,valleys, burnfreq, 
                                          CEC, PA, woody),
                    stars = T, ellipses = F)



# analyse the model (response ~ predictors) with a multiple regression approach
#this is "stupid model"
multreg_std <- lm(LF_N ~ RES_LHU + BIOMASS + FIRE_FRQ + NMS, data= Anderson2007std)
summary(multreg_std)
#influences not significant
#this model does not consider how the predictors influence each other


# visualization of the result: 
#browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model
#model is describe in a string '...'
Leaf_N_model <- 'LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS
                BIOMASS~FIRE_FRQ+RES_LHU
                NMS~FIRE_FRQ+RES_LHU'
Leaf_N_model
Leaf_N_fit <- lavaan::sem(Leaf_N_model, data=Anderson2007std)

# show the model results
summary(Leaf_N_fit, standardized=T, fit.measures=T, rsquare=T)
# CFI=0.995 and TLI=0.953 -> GOOD MODEL!!
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
