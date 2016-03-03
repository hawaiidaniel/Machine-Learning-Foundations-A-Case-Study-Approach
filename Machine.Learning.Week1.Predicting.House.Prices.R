library("caret")
data=read.csv("https://s3-ap-northeast-1.amazonaws.com/ldktorage/coursera_dato/home_data.csv")
plot(x=data[,6],y=data[,3],type="p")
inTrain=createDataPartition(data$price,p=0.8,list=F)
training=data[inTrain,]
testing=data[-inTrain,]
mod=lm(price ~ sqft_living, data=training)
summary(mod)
pred=predict(mod,newdata=testing)
#confusionMatrix(mod$finalModel,pred)
qplot(price,pred,data=testing)
