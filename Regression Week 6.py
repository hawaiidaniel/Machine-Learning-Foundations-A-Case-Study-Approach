import numpy as np
import pandas as pd

dtype_dict = {'bathrooms':float, 'waterfront':int, 'sqft_above':int,
              'sqft_living15':float, 'grade':int, 'yr_renovated':int,
              'price':float, 'bedrooms':float, 'zipcode':str,
              'long':float, 'sqft_lot15':float, 'sqft_living':float,
              'floors':float, 'condition':int, 'lat':float, 'date':str,
              'sqft_basement':int, 'yr_built':int, 'id':str,
              'sqft_lot':int, 'view':int}
sales = pd.read_csv('kc_house_data_small.csv', dtype=dtype_dict)
training = pd.read_csv('kc_house_data_small_train.csv', dtype=dtype_dict)
testing = pd.read_csv('kc_house_data_small_test.csv', dtype=dtype_dict)
validation = pd.read_csv('kc_house_data_validation.csv', dtype=dtype_dict)

def get_numpy_data(data_frame, features, output):
    data_frame['constant']=1
    features=['constant']+features
    feature_matrix=data_frame[features]
    output_array=data_frame[output]
    return (feature_matrix, output_array)
    
    
def normalize_features(feature_matrix):
    norms = np.linalg.norm(feature_matrix, axis=0)
    normalized_features = feature_matrix / norms
    return (normalized_features, norms)

feature_list = ['bedrooms',  
                'bathrooms',  
                'sqft_living',  
                'sqft_lot',  
                'floors',
                'waterfront',  
                'view',  
                'condition',  
                'grade',  
                'sqft_above',  
                'sqft_basement',
                'yr_built',  
                'yr_renovated',  
                'lat',  
                'long',  
                'sqft_living15',  
                'sqft_lot15']    

features_train, output_train=get_numpy_data(training,feature_list,'price')
features_test, output_test=get_numpy_data(testing,feature_list,'price')
features_valid, output_valid=get_numpy_data(validation,feature_list,'price')

features_train, norms = normalize_features(features_train)
features_test = features_test / norms
features_valid = features_valid / norms

##Q1 What is the Euclidean distance between the query house and the 10th house of the training set?
dist=np.sqrt(np.sum((features_test.iloc[0]-features_train.iloc[9])**2))
##Q2 Among the first 10 training houses, which house is the closest to the query house?
dist2=[]
closest=None
closest_index=None
for i in xrange(10):
    x=np.sqrt(np.sum((features_test.iloc[0]-features_train.iloc[i])**2))
    dist2.append(x)
    if closest is None or closest>x:
        closest=x
        closest_index=i

#Perform 1-nearest neighbor regression
diff=[]
for i in xrange(5527):
    x = features_train.iloc[i]-features_test.iloc[0]
    diff.append(x)
diff=pd.DataFrame(diff)
distances=np.sqrt(np.sum(diff**2, axis=1))


def compute_distances(features_instances, features_query):
    diff=[]
    closest=None
    closest_index=None
    for i in xrange(len(features_instances)):
        x = np.sqrt(np.sum((features_instances.iloc[i]-features_query)**2))
        diff.append(x)
    #diff=pd.DataFrame(diff)
    #distances=np.sqrt(np.sum(diff**2, axis=1))
        if closest is None or closest>x:
            closest=x
            closest_index=i
    return (closest_index,closest)
    

    
    
    
    
    