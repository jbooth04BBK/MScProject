import pandas as pd
import sys
import os
import csv

import Create_HAS_Tables

def modify_csv(orig_file_name):

    destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"
    file_ext = ".csv"

    # Read the data
    data = pd.read_csv(destination_folder + orig_file_name + file_ext)

    ignore_cols = ['event_id','event_start_date','age_category','age_in_days','gestation_at_delivery_in_days','case_id','cod2_summ','include_in_study']
    class_col = 'cod2_summ'

    amend_cols = []

    for cname in data.columns:
        if cname in ignore_cols:
            amend_cols.append([cname,'ignore'])
        elif data[cname].dtype in ['int64', 'float64']:
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
        if amend_col[1] == 'categorical':
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
                xml_row.append((col_name,6))

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
                        out_row.append("{:.4f}".format(((row.loc[col_name] - mean)/std)))
                else:
                    if col_name == class_col:
                        out_row.append(int(row.loc[col_name][1:]) - 1) # want classification to be 0 or 1
                    else:
                        if col_name in ['age_in_days','gestation_at_delivery_in_days'] and pd.isna(row.loc[col_name]):
                            out_row.append(row.loc[col_name]) # outputs nan
                            # out_row.append(-99)
                        else:
                            out_row.append(row.loc[col_name])

        writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")

def main():

    # modify_csv('rdv_study_ext')

    # modify_csv('rdv_study_int1')

    # modify_csv('rdv_study_int1_x')

    # modify_csv('rdv_study_int2')

    # modify_csv('rdv_study_int3')

    modify_csv('rdv_study_int2_s')

    modify_csv('rdv_study_int3_s')

if __name__ == "__main__":
    main()

