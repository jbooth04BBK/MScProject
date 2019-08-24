import pandas as pd
import sys
import os
import csv
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

import Create_HAS_Tables
import create_rdvs

def get_lrmodel_results(destination_folder, measures):

    lr_results = dict()

    orig_file_name = 'rdv_study_int1'
    file_ext = ".csv"

    # Read the data
    data = pd.read_csv(destination_folder + orig_file_name + file_ext)

    plot_maxcols = 4
    plot_maxrows = int(len(measures)/plot_maxcols)
    if plot_maxrows%2 != 0:
        plot_maxrows += 1

    fig, axs = plt.subplots(plot_maxrows, plot_maxcols, figsize=(10, 10))

    plot_index = 1
    plot_row = 0
    plot_col = 0

    for measure in measures:

        for sex in ['M','F','U']:

            if sex == 'M':
                point_colour = 'blue'
                line_colour = 'yellow'
            elif sex == 'F':
                point_colour = 'red'
                line_colour = 'green'
            else:
                point_colour = 'black'
                line_colour = 'white'

            for age in ['NN', 'IN']:

                # clean_data = data.dropna(how='any')
                clean_data = pd.concat([data['age_in_days'], data['sex'], data[measure]], axis=1, keys=['age_in_days', 'sex', measure])
                clean_data = clean_data[clean_data.sex == sex]
                if age == 'NN':
                    clean_data = clean_data[clean_data.age_in_days <= 100]
                else:
                    clean_data = clean_data[clean_data.age_in_days > 100]

                clean_data = clean_data.dropna()

                X = clean_data['age_in_days'].values.reshape(-1, 1)
                y = clean_data[measure].values.reshape(-1, 1)

                if len(X) > 0:
                    regressor = LinearRegression()
                    regressor.fit(X, y)  # training the algorithm

                    # To retrieve the intercept:
                    # print(measure, len(clean_data), regressor.intercept_, regressor.coef_)

                    # plot linear regression
                    # Plot outputs
                    y_pred = regressor.predict(X)
                    # Only want one set for legend
                    if age == 'NN':
                        axs[plot_row, plot_col].scatter(X, y, s = 2, color = point_colour, label = sex)
                        axs[plot_row, plot_col].plot(X, y_pred, color = line_colour, linewidth = 2, label = sex)
                    else:
                        axs[plot_row, plot_col].scatter(X, y, s = 2, color = point_colour)
                        axs[plot_row, plot_col].plot(X, y_pred, color=line_colour, linewidth=2)

                    lr_results[measure + "_" + sex + "_" + age] = (len(clean_data), regressor.intercept_[0], regressor.coef_[0][0])

        axs[plot_row, plot_col].set_title(str(measure))

        if str(measure) in ["heart_weight", "pancreas_weight", "thymus_weight", "spleen_weight", "comb_kidney_weight"]:
            axs[plot_row, plot_col].set_ylim(0,200)
        elif str(measure) in ["comb_adrenal_weight", "thyroid_weight"]:
            axs[plot_row, plot_col].set_ylim(0,50)

        plot_index += 1

        if plot_index % plot_maxcols == 0:
            plot_row += 1
            plot_col = 0
        else:
            plot_col += 1

    plt.suptitle('Visualisation of Normal Measurement Predictions')
    plt.tight_layout()
    plt.show()

    return lr_results


def modify_csv(destination_folder, orig_file_name, lr_results, measures):

    file_ext = ".csv"

    # Read the data
    data = pd.read_csv(destination_folder + orig_file_name + file_ext)

    ignore_cols = ['event_id','event_start_date','age_category','age_in_days','gestation_at_delivery_in_days','case_id','cod2_summ','include_in_study']
    class_col = 'cod2_summ'

    print("")
    print("Adjusting columns for age_in_days")
    row_counter = 0

    # Adjust measures columns - normaise for age
    for index, row in data.iterrows():

        row_counter += 1
        sys.stdout.write("\r \r {0}".format(str(row_counter)))
        sys.stdout.flush()

        for cname in data.columns:
            if cname in measures:
                if not pd.isna(row.loc[cname]):
                    # Add sex to results name
                    if row.loc["sex"] in ['M','F','U']:
                        rname = cname + "_" + row.loc["sex"]
                    elif row.loc["sex"] in ['b', 'B']:
                        rname = cname + "_M"
                    elif row.loc["sex"] in ['g','G']:
                        rname = cname + "_F"
                    else:
                        rname = cname + "_U"

                    # Add age to results name
                    if row.loc["age_in_days"] <= 100:
                        rname += "_NN" # NeoNates
                    else:
                        rname += "_IN" # Infants

                    if rname in lr_results:
                        intercept = lr_results[rname][1]
                        coefficient = lr_results[rname][2]
                        actual = row.loc[cname]
                        predicted = intercept + (row.loc["age_in_days"] * coefficient)
                        value = (abs(predicted - actual) / predicted)
                        data.at[index, cname] =  value
                    else:
                        data.at[index, cname] =  None


    amend_cols = []

    for cname in data.columns:
        if cname in ignore_cols:
            amend_cols.append([cname,'ignore'])
        elif data[cname].dtype in ['int64', 'float64']:
            # Divide all values by age_in_days
            #   NB if age_in_days not available replace with nan
            amend_cols.append([cname,'numeric', data[cname].mean(), data[cname].std(), data[cname].min(), data[cname].max()])
        elif data[cname].dtype in ['object']:
            if data[cname].nunique() <= 10:
                list = data[cname].sort_values().unique()
                amend_cols.append([cname, 'categorical', list])
            else:
                amend_cols.append([cname, 'ignore'])
        else:
            amend_cols.append([cname, 'ignore'])

    file_name = orig_file_name + "_adj"

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + Create_HAS_Tables.file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    writer = csv.writer(file, quoting=csv.QUOTE_MINIMAL)

    # Get column Headings

    out_row = []
    xml_row = []

    for amend_col in amend_cols:
        if amend_col[1] == 'ignore':
            col_name = amend_col[0]
            if amend_col[0] in ('event_id','age_in_days','gestation_at_delivery_in_days','case_id'):
                out_row.append(col_name)
                xml_row.append((col_name, 3))
            elif amend_col[0] == "event_start_date":
                out_row.append(col_name)
                xml_row.append((col_name, 6))
            else:
                out_row.append(col_name)
                xml_row.append((col_name, 8))
        elif amend_col[1] == 'categorical':
            for new_col in amend_col[2]:
                if isinstance(new_col,float):
                    col_name = amend_col[0] + '_nan'
                else:
                    col_name = amend_col[0] + '_' + new_col.lower()
                out_row.append(col_name)
                xml_row.append((col_name, 3))
        else:
            col_name = amend_col[0]
            out_row.append(col_name)
            if amend_col[1] == 'numeric':
                xml_row.append((col_name,4))
            else:
                xml_row.append((col_name,8))

    writer.writerow(out_row)

    row_counter = 0
    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    for index, row in data.iterrows():
        row_counter += 1

        sys.stdout.write("\r \r {0}".format(str(row_counter)))
        sys.stdout.flush()

        out_row = []

        for amend_col in amend_cols:
            col_name = amend_col[0]
            if amend_col[1] == 'categorical':
                for category in amend_col[2]:
                    if isinstance(category,float):
                        if pd.isna(row.loc[col_name]):
                            out_row.append(1)
                        else:
                            out_row.append(0)
                    else:
                        if row.loc[col_name] == category:
                            out_row.append(1)
                        else:
                            out_row.append(0)
            else:
                if amend_col[1] == 'numeric':
                    if pd.isna(row.loc[col_name]):
                        out_row.append(row.loc[col_name]) # outputs nan
                        # out_row.append(-99)
                    else:
                        mean = amend_col[2]
                        std = amend_col[3]
                        # Z score, standardisation - not required if age_normalised
                        # out_row.append("{:.4f}".format(((row.loc[col_name] - mean)/std)))
                        out_row.append("{:.4f}".format(row.loc[col_name]))
                else:
                    if col_name == class_col:
                        out_row.append(int(row.loc[col_name][1:]) - 1) # want classification to be 0 or 1
                    else:
                        if col_name in ['age_in_days','gestation_at_delivery_in_days'] and pd.isna(row.loc[col_name]):
                            out_row.append(row.loc[col_name]) # outputs nan
                        elif col_name in ['age_in_days', 'gestation_at_delivery_in_days']:
                                out_row.append("{:.0f}".format(row.loc[col_name]))  # outputs nan
                        else:
                            out_row.append(row.loc[col_name])

        writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")

    root_file_name = file_name

    file_name = root_file_name + "_columns"

    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + Create_HAS_Tables.file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    writer = csv.writer(file, quoting=csv.QUOTE_MINIMAL)

    out_row = ["column_name","process","mean","st_dev","min","max"]
    writer.writerow(out_row)

    for amend_col in amend_cols:

        cname = amend_col[0]

        if amend_col[1] == 'numeric':
            out_row = [cname, amend_col[1], data[cname].mean(), data[cname].std(), data[cname].min(), data[cname].max()]
        else:
            out_row = [cname, amend_col[1], "nan", "nan", "nan", "nan"]

        writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")

    file_name = root_file_name + "_lrparams"

    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + Create_HAS_Tables.file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    writer = csv.writer(file, quoting=csv.QUOTE_MINIMAL)

    out_row = ["column_name","observations","intercept","coeficient"]
    writer.writerow(out_row)

    # Output linear regression parameters
    for key in lr_results:
        out_row = [key, lr_results[key][0], lr_results[key][1], lr_results[key][2]]
        writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")

    #Create XML file
    create_rdvs.create_tdf_file(xml_row, destination_folder, root_file_name, "nan")


def main():

    destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"

    measures_all = ["body_weight","head_circumference","crown_rump_length","body_length","foot_length","heart_weight","comb_lung_weight","liver_weight","pancreas_weight","thymus_weight","spleen_weight","comb_adrenal_weight","thyroid_weight","comb_kidney_weight","brain_weight"]

    lr_results = get_lrmodel_results(destination_folder, measures_all)

    # for key in lr_results:
    #     print(key, lr_results[key])

    measures_ext = ["body_weight","head_circumference","crown_rump_length","body_length","foot_length"]

    modify_csv(destination_folder, 'rdv_study_ext', lr_results, measures_ext)

    modify_csv(destination_folder, 'rdv_study_int1', lr_results, measures_all)

    # modify_csv(destination_folder, 'rdv_study_int1_x', lr_results, measures_all)

    modify_csv(destination_folder, 'rdv_study_int2', lr_results, measures_all)

    modify_csv(destination_folder, 'rdv_study_int3', lr_results, measures_all)

    # modify_csv(destination_folder, 'rdv_study_int2_s', lr_results, measures_all)

    # modify_csv(destination_folder, 'rdv_study_int3_s', lr_results, measures_all)

if __name__ == "__main__":
    main()

