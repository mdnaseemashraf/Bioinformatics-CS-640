# -*- coding: utf-8 -*-
"""
Machine Learning
"""
from scipy.io.arff import loadarff
import matplotlib.pyplot as plt
import numpy as np
import scipy as sp

dataset = loadarff(open('disease2a.arff','r'))
target = np.array(dataset[0]['class']) 
train = np.array(dataset[0][['location1', 'location2', 'location3', 'location4', 'location5', 'location6', 'location7', 'location8', 'location9', 'location10']])

##Counts of 0,1,2 for each variable
##Histogram/Summary
for prednum in range(0,10):
    zero_count = 0
    one_count = 0
    two_count = 0
    for counter in range(0,2000):
        row = train[counter]
        item = int(row[prednum])
        
        if(item == 0):
            zero_count = zero_count+1
        elif(item == 1):
            one_count = one_count+1
        else:
            two_count = two_count+1
     
        vals = [zero_count,one_count,two_count]
    
    objects = ('0:'+str(vals[0]), '1:'+str(vals[1]), '2:'+str(vals[2]))
    y_pos = np.arange(len(objects)) 
    
    plt.bar(y_pos, vals, align='center', alpha=0.5)
    plt.xticks(y_pos, objects)
    plt.ylabel('Frequency')
    plt.title('location'+str(prednum+1)+'Predictor Values')
     
    plt.show()
    
##SVM
#train = np.array(dataset[0][['location1', 'location2', 'location3', 'location4', 'location5', 'location6', 'location7', 'location8', 'location9', 'location10']])

loc1 = np.array(dataset[0]['location1']) 
loc2 = np.array(dataset[0]['location2']) 
loc3 = np.array(dataset[0]['location3']) 
loc4 = np.array(dataset[0]['location4']) 
loc5 = np.array(dataset[0]['location5']) 
loc6 = np.array(datset[0]['location6']) 
loc7 = np.array(dataset[0]['location7']) 
loc8 = np.array(dataset[0]['location8']) 
loc9 = np.array(dataset[0]['location9']) 
loc10 = np.array(dataset[0]['location10'])

disease_status = np.array(dataset[0]['class']) 
train = np.array(dataset[0][['location1', 'location2', 'location3', 'location4', 'location5', 'location6', 'location7', 'location8', 'location9', 'location10']])
locations_X = np.asarray(train.tolist(), dtype=np.float32)
#locations_X = np.asarray(train.tolist(), dtype="category")
 
np.random.seed(0)
indices = np.random.permutation(len(locations_X))

locations_X_train = locations_X[indices[:-10]] 
disease_status_train = disease_status[indices[:-10]] 
locations_X_test = locations_X[indices[-10:]] 
disease_status_test = disease_status[indices[-10:]] 
 
from sklearn import svm 
svc = svm.SVC(kernel='linear')
svc.fit(locations_X_train, disease_status_train) 
 
disease_status_predict = svc.predict(locations_X_test) 
print(disease_status_test)
print(disease_status_predict)
 

