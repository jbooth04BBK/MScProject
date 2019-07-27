import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics

# Load the diabetes dataset
destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"
file_ext = ".csv"

orig_file_name = 'rdv_study_int1'

# Read the data
data = pd.read_csv(destination_folder + orig_file_name + file_ext)

measure = "heart_weight"

# clean_data = data.dropna(how='any')
clean_data = pd.concat([data['age_in_days'], data[measure]], axis=1, keys=['age_in_days', measure])
clean_data = clean_data.dropna()

X = clean_data['age_in_days'].values.reshape(-1,1)
y = clean_data[measure].values.reshape(-1,1)

regressor = LinearRegression()
regressor.fit(X, y) #training the algorithm

#To retrieve the intercept:
print(regressor.intercept_)

#For retrieving the slope:
print(regressor.coef_)

# y_pred = regressor.predict(X_test)
#
# df = pd.DataFrame({'Actual': y_test.flatten(), 'Predicted': y_pred.flatten()})
# print(df)
#
# df1 = df.head(25)
# df1.plot(kind='bar',figsize=(16,10))
# plt.grid(which='major', linestyle='-', linewidth='0.5', color='green')
# plt.grid(which='minor', linestyle=':', linewidth='0.5', color='black')
# plt.show()
