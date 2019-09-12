import os
import datetime
import csv
import re

def file_time_stamp():
    return str(datetime.datetime.now())[:19].replace("-", "").replace(":", "").replace(" ", "_")

def left(s, amount):
    return s[:amount]

def right(s, amount):
    return s[-amount:]

def mid(s, offset, amount):
    return s[offset:offset + amount]

def main():

    source_folder = r"C:\Users\BoothJ1\PycharmProjects\MScProject"
    destination_folder = "C:\\Users\\BoothJ1\\PycharmProjects\\"
    file_name = "MScProject_files"
    file_ext = ".csv"

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file_csv = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    writer = csv.writer(file_csv, quoting=csv.QUOTE_MINIMAL)

    print("%s open" % destination_folder + file_name + file_ext)

    row_counter = 0
    cnt_root = ""

    for root, dirs, files in os.walk(source_folder):

        for file in files:
            inc_file = True
            # ignore folders .git and .idea
            if root.find(".git") >= 0:
                inc_file = False
            elif root.find(".idea") >= 0:
                inc_file = False

            if inc_file:

                if cnt_root != root:
                    folders = root.split("\\")
                    folder_description = ""
                    if folders[-1] == "MScProject":
                        folder_description = "Python Scripts produced during the project"
                    if folders[-1] == "Data":
                            folder_description = "RDV CSV and XML files used to produced output for the project report"
                    elif folders[-1] == "Docs":
                            folder_description = "Project report files"
                    elif folders[-1] == "Images":
                            folder_description = "Images files used in project report"
                    elif folders[-1] == "run_01":
                        folder_description = "Support files output by R scripts produced during a complete run and used in the project report"
                    elif folders[-1] == "RCode":
                            folder_description = "R Scripts produced during the project"
                    out_row = [ root, "", "", folder_description]
                    writer.writerow(out_row)
                    cnt_root = root

                row_counter += 1

                filename, file_extension = os.path.splitext(os.path.join(root,file))
                # remove file extension from file
                filename = file[:len(file)-len(file_extension)].lower()
                file_extension = file_extension.lower()
                file_description = ""

                if filename == "create_has_tables":
                    file_description = "Python script to create and populate HAS tables"
                elif filename == "create_rdvs":
                    file_description = "Python script to create RDV CSV and XML files"
                elif filename == "modify_csv_data":
                    file_description = "Python script to modify original RDV files creates revised CSV and XML file"
                elif filename == "modify_events":
                    file_description = "Python script to add new event attributes to existing events"
                elif filename == "dtree_study":
                    file_description = "R script  - Decision tree model function"
                elif filename == "rforest_study":
                    file_description = "R script  - Random Forest model function"
                elif filename == "gboost_study":
                    file_description = "R script  - XGBoost model function"
                elif filename == "run_models":
                    file_description = "R script  - Runs models for multiple random.seeds and combines results"
                elif filename == "study_functions":
                    file_description = "R script  - R functions used across all models"
                elif file_extension == ".py":
                    file_description = "Python script"
                elif file_extension == ".r":
                    file_description = "R script"
                elif file_extension == ".xml":
                    file_description = "Table definition file"
                elif file_extension == ".png":
                    file_description = "Image file"
                elif file_extension == ".txt":
                    file_description = "Text file"
                elif file_extension == ".docx":
                    file_description = "Word Document file"
                elif file_extension == ".pptx":
                    file_description = "Power point file"
                elif file_extension == ".csv":
                    file_description = "Comma separated text data file"
                elif file_extension == ".txt":
                    file_description = "Text file"
                elif file_extension == ".md":
                    file_description = "Markup text file"

                out_row = [root, filename, file_extension, file_description]
                writer.writerow(out_row)

    file_csv.close()

    print("")
    print("%s closed" % destination_folder + file_name + file_ext)

if __name__ == "__main__":
    main()

