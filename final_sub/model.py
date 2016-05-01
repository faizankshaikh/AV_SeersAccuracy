__author__ = ['jalFaizy', 'anchal']

# import modules
import numpy as np
import pandas as pd
from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_selection import RFE

# read data
data = pd.read_csv('data_cleaned.csv')

# make data ready
data_x = data.drop(['Target', 'Client_ID'], axis = 1)
data_y = data.Target

# set train and test
train_x = data_x[data_x['X2006'] == 0].values
train_y = data_y[data_x['X2006'] == 0].values
test_x = data_x[data_x['X2006'] == 1].values
test_y = data_y[data_x['X2006'] == 1].values

# set model
nb = BernoulliNB(250)

# select features
rfe = RFE(nb, n_features_to_select=39)
rfe.fit(train_x, train_y);

# make predictions
pred = rfe.predict_proba(data_x)

# create submission
pd.DataFrame({'Client_ID':data.Client_ID, 'Cross_Sell':pred[:, 1]}).to_csv('sub_final.csv', index=False)