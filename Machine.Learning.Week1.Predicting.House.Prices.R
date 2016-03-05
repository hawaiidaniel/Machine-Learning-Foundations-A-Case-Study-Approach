library("caret")
data=read.csv("https://s3-ap-northeast-1.amazonaws.com/ldktorage/coursera_dato/home_data.csv")
inTrain=createDataPartition(data$price,p=0.8,list=F)
training=data[inTrain,]
testing=data[-inTrain,]
mod=lm(price ~ sqft_living, data=training)
pred=predict(mod,newdata=testing)
postResample(pred,testing$price)
#Training data and fitted values
plot(x=training$sqft_living,y=training[,3],type="p",col="red")
lines(training[,6],mod$fitted.values,col="blue")
#Test data and prediction
plot(x=testing$sqft_living,y=testing[,3],type="p",col="red")
lines(testing[,6],pred,col="blue")
#Getting model coefficients
summary(mod)


#Explore other features
# training_new=training[c('price','bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors', 'zipcode')]
# testing_new=testing[c('price','bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors', 'zipcode')]
boxplot(price ~ zipcode, data=training_new)

mod_new=train(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+zipcode, data=training,method="glm")
pred_new=predict(mod_new,newdata=testing)
postResample(pred_new,testing$price)

house1=data[data['id']==5309101200,]
house1['price']
#Predict a specific house with 1st method
predict(mod,house1)
#Predict a specific house with 2nd method
predict(mod_new,house1)



house2=data[data['id']==1925069082,]
house2['price']
#Predict a specific house with 1st method
predict(mod,house2)
#Predict a specific house with 2nd method
predict(mod_new,house2)



bill_gates = data.frame('bedrooms'=8,'bathrooms'=25, 
                        'sqft_living'=50000, 
                        'sqft_lot'=225000,
                        'floors'=4, 
                        'zipcode'=98039, 
                        'condition'=10, 
                        'grade'=10,
                        'waterfront'=1,
                        'view'=4,
                        'sqft_above'=37500,
                        'sqft_basement'=12500,
                        'yr_built'=1994,
                        'yr_renovated'=2010,
                        'lat'=47.627606,
                        'long'=-122.242054,
                        'sqft_living15'=5000,
                        'sqft_lot15'=40000)

predict(mod,bill_gates)
#Predict a specific house with 2nd method
predict(mod_new,bill_gates)




#RMSE
#sqrt(sum(residuals(mod_new)^2) / length(training$id))
#Quiz
library("caret")
data=read.csv("https://s3-ap-northeast-1.amazonaws.com/ldktorage/coursera_dato/home_data.csv")
#1.
data1=subset(data,select=c(price,zipcode))
meanbyzip=aggregate(data1[,1], by=list(data1$zipcode),FUN=mean)
max_zip=max(meanbyzip[,2])

#2
data2=subset(data,sqft_living>2000 & sqft_living<=4000,select = id:sqft_lot15)

#3
my_features = c('bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors', 'zipcode')
advanced_features = c('bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors', 'zipcode',
    'condition', # condition of house				
    'grade', # measure of quality of construction				
    'waterfront', # waterfront property				
    'view', # type of view				
    'sqft_above', # square feet above ground				
    'sqft_basement', # square feet in basement				
    'yr_built', # the year built				
    'yr_renovated', # the year renovated				
    'lat', 'long', # the lat-long of the parcel				
    'sqft_living15', # average sq.ft. of 15 nearest neighbors 				
    'sqft_lot15')

inTrain=createDataPartition(data$price,p=0.8,list=F)
training=data[inTrain,]
testing=data[-inTrain,]
