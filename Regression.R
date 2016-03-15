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

##Regression Week 1: Simple Linear Regression Assignment##

training=read.csv("kc_house_train_data.csv")
mod=lm(price~sqft_living, data=training)
intercept=mod$coefficients[[1]]
slope=mod$coefficients[[2]]
predicted_output=data.frame()
for (i in seq_along(training$id)) {
  predicted_output=rbind(predicted_output,(intercept+slope*training$sqft_living[i]))
}
price_6=intercept+slope*2650
avg=mean(training$price)
RSS=0
for (i in 1:17384) {
  RSS=RSS+((predicted_output[i,]-avg)^2)
}
#sum((predict(lm1) - mean(movies$score))^2) 
#anova(mod)

#800000=intercept+slope*sqft
estimated_sqft=(800000-intercept)/slope


training=read.csv("kc_house_train_data.csv")
testing=read.csv("kc_house_test_data.csv")
mod=lm(price~sqft_living, data=training)
intercept=mod$coefficients[[1]]
slope=mod$coefficients[[2]]
predicted_output=data.frame()
for (i in 1:4229) {
  predicted_output=rbind(predicted_output,(intercept+slope*testing$sqft_living[i]))
}
avg=mean(testing$price)
RSS=0
for (i in 1:4229) {
  RSS=RSS+((predicted_output[i,]-avg)^2)
}
#RSS=2.75555e+14

training=read.csv("kc_house_train_data.csv")
testing=read.csv("kc_house_test_data.csv")
mod=lm(price~bedrooms, data=training)
intercept=mod$coefficients[[1]]
slope=mod$coefficients[[2]]
predicted_output=data.frame()
for (i in seq_along(testing$id)) {
  predicted_output=rbind(predicted_output,(intercept+slope*testing$bedrooms[i]))
}
avg=mean(testing$price)
RSS=0
for (i in 1:4229) {
  RSS=RSS+((predicted_output[i,]-avg)^2)
}
#RSS=7.184082e+13


data=read.csv("kc_house_data.csv")
bedrooms_squared=(data$bedrooms)^2
bed_bath_rooms=(data$bedrooms)*(data$bathrooms)
log_sqft_living = log(data$sqft_living)
lat_plus_long=data$lat+data$long
new_data=cbind(data,bedrooms_squared,bed_bath_rooms,log_sqft_living,lat_plus_long)
set.seed(0)
inTrain=createDataPartition(new_data$price,p=0.6,list=F)
training=new_data[inTrain,]
testing=new_data[-inTrain,]
mean_bedrooms_squared=colMeans(testing[,c(22:25)])

mod1=lm(price~sqft_living+bedrooms+bathrooms+lat+long,data=training)
mod2=lm(price~sqft_living+bedrooms+bathrooms+lat+long+bed_bath_rooms,data=training)
mod3=lm(price~sqft_living+bedrooms+bathrooms+lat+long+bed_bath_rooms+bedrooms_squared+log_sqft_living+lat_plus_long,data=training)

pred1=predict(mod1,testing)
pred2=predict(mod2,testing)
pred3=predict(mod3,testing)

mean_price=mean(testing$price)
sum((pred1 - mean_price)^2) 
sum((pred2 - mean_price)^2) 
sum((pred3 - mean_price)^2) 

