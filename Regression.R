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



import pandas 
import numpy as np
import matplotlib.pylab as plt
from sklearn.linear_model import Ridge
sales=pandas.read_csv('C:\Users\wli\Downloads\kc_house_train_data.csv')
test=pandas.read_csv('C:\Users\wli\Downloads\kc_house_test_data.csv')
#training_new=training['sqft_living']

def polynomial_frame(feature, degree):
    poly_frame=pandas.DataFrame()
    poly_frame['power_1']=feature
    
    if degree>1:
        for power in range(2,degree+1):
            name='power_'+str(power)
            poly_frame[name]=feature.apply(lambda x: x**power)
    return poly_frame

training=polynomial_frame(sales['sqft_living'],15)
training['price']=sales['price']
my_features=['power_1','power_2','power_3','power_4','power_5','power_6',
'power_7','power_8','power_9','power_10','power_11','power_12',
'power_13','power_14','power_15']

model=Ridge(alpha=1000,normalize=False)
model.fit(training[my_features],training['price'])








import pandas as pd
from sklearn import linear_model
import numpy as np
from sklearn.grid_search import GridSearchCV
import matplotlib.pyplot as plt

def polynomial_sframe(feature, degree):
    poly_frame=pd.DataFrame()
    poly_frame['power_1']=feature
    
    if degree>1:
        for power in range(2,degree+1):
            name='power_'+str(power)
            poly_frame[name]=feature.apply(lambda x: x**power)
    return poly_frame

dtype_dict = {'bathrooms':float, 'waterfront':int, 'sqft_above':int, 
'sqft_living15':float, 'grade':int, 'yr_renovated':int, 'price':float, 
'bedrooms':float, 'zipcode':str, 'long':float, 'sqft_lot15':float, 
'sqft_living':float, 'floors':float, 'condition':int, 'lat':float, 'date':str, 
'sqft_basement':int, 'yr_built':int, 'id':str, 'sqft_lot':int, 'view':int}

sales = pd.read_csv('kc_house_data.csv', dtype=dtype_dict)
sales = sales.sort(['sqft_living','price'])
l2_small_penalty = 1.5e-5

poly15_data = polynomial_sframe(sales['sqft_living'], 15) # use equivalent of `polynomial_sframe`
model = linear_model.Ridge(alpha=l2_small_penalty, normalize=True)
model.fit(poly15_data, sales['price'])
#model.coef_
'''
array([  1.24873306e+02,  -4.77376011e-02,   3.01446238e-05,
        -2.44419942e-09,  -1.94153675e-13,   8.54085686e-18,
         1.51142121e-21,   8.27979094e-26,   6.52603100e-31,
        -3.27895017e-34,  -3.87962315e-38,  -2.72437650e-42,
        -1.07790800e-46,   3.78242694e-51,   1.39790296e-54])
'''
train_valid_shuffled = pd.read_csv('wk3_kc_house_train_valid_shuffled.csv', 
                                   dtype=dtype_dict)
test = pd.read_csv('wk3_kc_house_test_data.csv', dtype=dtype_dict)
n = len(train_valid_shuffled)
k = 10 # 10-fold cross-validation

for i in xrange(k):
    start = (n*i)/k
    end = (n*(i+1))/k-1
    print i, (start, end)

train_valid_shuffled[0:10] # select rows 0 to 9


def k_fold_cross_validation(k, l2_penalty, data, my_features, output):
    n=len(data)
    validation_err=[]
    for i in xrange(k):
        start = (n*i)/k
        end = (n*(i+1))/k-1
        validation=data[start:end+1]    
        training=data[0:start].append(data[end+1:n])
        model=linear_model.Ridge(l2_penalty)
        model.fit(training[my_features],training[output])
        RSS=sum((model.predict(validation[my_features])-validation['price'])**2)
        validation_err.append(RSS)
    return sum(validation_err)/k

data15 = polynomial_sframe(train_valid_shuffled['sqft_living'], 15)
my_features = list(data15.columns.values)
data15['price'] = train_valid_shuffled['price']

total_val=[]
best_l2=[]
best_err=None
l2_penalty=np.logspace(3, 9, num=13)
for l2 in l2_penalty:
    val_err=k_fold_cross_validation(10,l2,data15,my_features,'price')
    total_val.append(val_err)
    print str(l2)+' and '+str(val_err)
    if best_err is None or best_err>val_err:
        best_l2=l2
        best_err=val_err
print 'best '+str(best_l2)+' and '+str(best_err)
plt.plot(l2_penalty[2:12],total_val[2:12],'k-')
plt.xlabel('$\L2_penalty$')
plt.ylabel('K-fold cross validation error')
plt.xscale('log')
plt.yscale('log')


model_python=RidgeCV(l2_penalty)
model_python.fit(data15[my_features],
                 data15['price'])

'''
rss=[]
for alpha in alphas:
    model=Ridge(alpha)
    model.fit(training[my_features],training['price'])
    pred=model.predict(testing)
    val_err=sum((pred-training['price'])**2)
    rss.append(val_err)

plt.plot(alphas,rss,'k-')
plt.xlabel('$\L2_penalty$')
plt.ylabel('K-fold cross validation error')
plt.xscale('log')
plt.yscale('log')'''


tuned_parameters={'alpha':l2_penalty}
model2=GridSearchCV(Ridge(),tuned_parameters,cv=10)
model2.fit(data15[my_features], data15['price'])



















