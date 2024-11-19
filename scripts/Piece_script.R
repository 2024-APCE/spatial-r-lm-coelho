
woody_model2 <- 'woody~dist2river+elevation+burnfreq+CEC
                rainfall~elevation
                burnfreq~dist2river
                CEC~dist2river+rainfall+burnfreq+PA'
woody_model2
woody_fit2 <- lavaan::sem(woody_model2, data=SEMdatastd)

# show the model results
summary(woody_fit2, standardized=T, fit.measures=T, rsquare=T)





renv::restore()

# read the data from the google docs link:
SEMdata <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR-cr8z8Pye7gakP0ihX5SUtNLU8h_evJAySe4ZcnRL1d2HAoOkHKmDe9cM6uA4oC1A4QRArIlxFX5V/pub?gid=1240187235&single=true&output=csv")
names(SEMdata)


# Model 1: woody predicted by burnfreq and CEC   ### SKEWED DATA - GLM - POISSON?!
piece_woody <- glm(woody ~  CEC + burnfreq, 
                  data = SEMdata)
summary(piece_woody)

woody1<-ggplot(data=SEMdata,aes(x=CEC,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
#              method.args=list(family=Gamma(link="log")), ### SHOULD I USE THIS??
              formula= y~x,
              se=T)
woody1

woody2<-ggplot(data=SEMdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
woody2



# MODEL 2: burnfreq predicted by dist2river
piece_burnfreq_init <- glm(burnfreq ~ dist2river, 
                           family=poisson, 
                           data = SEMdata)
# Calculate dispersion statistic
disp_stat <- summary(piece_burnfreq_init)$deviance / summary(piece_burnfreq_init)$df.residual
disp_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
piece_burnfreq <- MASS::glm.nb(burnfreq ~ dist2river, 
                               data = SEMdata)
summary(piece_burnfreq)

burnfreq1<-ggplot(data=SEMdata,aes(y=burnfreq,x=dist2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
burnfreq1



# MODEL 3: CEC predicted by rainfall, burnfreq and PA
piece_cec <- glm(CEC ~ rainfall + burnfreq + PA, 
                 data = SEMdata)
summary(piece_cec)

CEC1<-ggplot(data=SEMdata,aes(y=CEC,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
CEC1

CEC2<-ggplot(data=SEMdata,aes(y=CEC,x=PA))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
CEC2



# MODEL 4: rainfall predicted by elevation
piece_rainfall <- lm(rainfall ~  elevation, 
                  data = SEMdata)
summary(piece_rainfall)

rainfall1 <-ggplot(data=SEMdata,aes(x=elevation,y=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
rainfall1




##################################################
# combine the figures
library(patchwork)
all_plots<-woody1+woody2+burnfreq1+CEC1+CEC2+rainfall1 +
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
all_plots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_CorProtAr,
                                 model_rainfall)

# Summarize the SEM results
summary(psem_model)
