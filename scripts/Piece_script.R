
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


# Model 1: woody predicted by burnfreq and rainfall   ### SKEWED DATA - GLM - POISSON?!
piece_woody <- glm(woody ~  CEC +burnfreq + dist2river, 
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

woody3<-ggplot(data=SEMdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
woody3


