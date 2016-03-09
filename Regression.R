library("ggplot2")
library("caret")
dat=read.csv("Philadelphia_Crime_Rate_noNA.csv")
qplot(CrimeRate,HousePrice,data=dat)  #Relationship between crime rate & price
#Fit the regreesion model using crime as the feature
mod_crime=lm(HousePrice ~ CrimeRate,data=dat)
ggplot(data=dat,aes(CrimeRate,HousePrice))+geom_point()+
  stat_smooth(method="lm")

#Remove outlier
dat_new=dat[-which.max(dat[,"CrimeRate"]),]
mod_crime_new=lm(HousePrice ~ CrimeRate,data=dat_new)
# ggplot(data=dat_new,aes(CrimeRate,HousePrice))+geom_point()+
#   stat_smooth(method="lm")
plot(HousePrice~CrimeRate,data=dat_new)
abline(mod_crime_new)
