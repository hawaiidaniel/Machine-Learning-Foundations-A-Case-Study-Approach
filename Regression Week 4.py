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


















