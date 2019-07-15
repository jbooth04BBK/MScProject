import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split

destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"
file_ext = ".csv"
file_name = "rdv_study_int1"

# Read the data
data = pd.read_csv(destination_folder + file_name + file_ext)

print(data.head())

numerical_cols = [cname for cname in data.columns if data[cname].dtype in ['int64', 'float64']]

for numeric_col in numerical_cols:
    print(numeric_col, data[numeric_col].mean(), data[numeric_col].std(), data[numeric_col].min(), data[numeric_col].max())
print()

low_cardinality_cols = [cname for cname in data.columns if data[cname].nunique() < 10 and
                        data[cname].dtype == "object"]

for low_cardinality_col in low_cardinality_cols:
    list = data[low_cardinality_col].sort_values().unique()
    print(low_cardinality_col, data[low_cardinality_col].nunique(), list)
print()

for cname in data.columns:
    if data[cname].dtype in ['int64', 'float64']:
        print(cname,data[cname].dtype, data[cname].mean(), data[cname].std(), data[cname].min(), data[cname].max())
    elif data[cname].dtype in ['object']:
        if data[cname].nunique() <= 10:
            list = data[cname].sort_values().unique()
            print(cname, data[cname].nunique(), list)
        else:
            print(cname, data[cname].dtype, data[cname].nunique())
    else:
        print(cname, data[cname].dtype)


