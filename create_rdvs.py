import pyodbc
import pandas
import datetime
import decimal
import sys
import time
import csv
import os
import re

def camel_snake(text):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', text)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def get_column_heading(text):
    # change to lower case and converts camel to snake
    text = camel_snake(text)
    # remove multiple spaces
    text = " ".join(text.split())
    # change space to underscore
    text = text.replace(" ","_")
    # remove multiple underscores
    text = text.replace("__","_")
    return text

def file_time_stamp():
    return str(datetime.datetime.now())[:19].replace("-", "").replace(":", "").replace(" ", "_")

def create_rdv_complete(cnxn, crsr):

    '''

    :param cnxn: Connection to database
    :param crsr: Cursor for all SQL operations

    '''

    # Get list of event_ids
    SQLstring = "SELECT"
    SQLstring += "       ha_events.event_id "
    SQLstring += "FROM   ha_events "
    SQLstring += "ORDER BY "
    SQLstring += "       ha_events.event_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventRows = crsr.fetchall()

    # Get summary list of patient_ids
    SQLstring = "SELECT"
    SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label, "
    SQLstring += "       COUNT(*) AS records "
    SQLstring += "FROM   ((ha_events "
    SQLstring += "       INNER JOIN ha_patients "
    SQLstring += "               ON ha_events.patient_id = ha_patients.patient_id) "
    SQLstring += "       INNER JOIN ha_patient_attributes "
    SQLstring += "               ON ha_patients.patient_id = ha_patient_attributes.patient_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_patient_attributes.patient_attribute_type_concept_id = ha_concepts.concept_id "
    SQLstring += "GROUP BY "
    SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label "
    SQLstring += "ORDER BY "
    SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventPatientAttributeSummaryRows = crsr.fetchall()

    for EventPatientAttributeSummaryRow in EventPatientAttributeSummaryRows:
        print(EventPatientAttributeSummaryRow.label, EventPatientAttributeSummaryRow.records)


    # get patient attributes values

    SQLstring = "SELECT"
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.patient_id, "
    SQLstring += "       ha_events.event_type_concept_id, "
    SQLstring += "       ha_events.start_date, "
    SQLstring += "       ha_patients.sex, "
    SQLstring += "       ha_patient_attributes.patient_attribute_id, "
    SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_patient_attributes.value_numeric, "
    SQLstring += "       ha_concepts_1.code "
    SQLstring += "FROM   (((ha_events "
    SQLstring += "       INNER JOIN ha_patients "
    SQLstring += "               ON ha_events.patient_id = ha_patients.patient_id) "
    SQLstring += "       INNER JOIN ha_patient_attributes "
    SQLstring += "               ON ha_patients.patient_id = ha_patient_attributes.patient_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_patient_attributes.patient_attribute_type_concept_id = ha_concepts.concept_id) "
    SQLstring += "       LEFT JOIN ha_concepts AS ha_concepts_1 "
    SQLstring += "               ON ha_patient_attributes.value_concept_id = ha_concepts_1.concept_id"
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventPatientAttributeRows = crsr.fetchall()

    SQLstring = "SELECT "
    SQLstring += "       ha_event_attributes.event_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label, "
    SQLstring += "       COUNT(*) AS records "
    SQLstring += "FROM   (ha_events "
    SQLstring += "       INNER JOIN ha_event_attributes "
    SQLstring += "               ON ha_events.event_id = ha_event_attributes.event_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_event_attributes.event_attribute_type_concept_id = ha_concepts.concept_id "
    SQLstring += "GROUP BY "
    SQLstring += "       ha_event_attributes.event_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label "
    SQLstring += "ORDER BY "
    SQLstring += "       ha_event_attributes.event_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_concepts.label "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeSummaryRows = crsr.fetchall()

    for EventAttributeSummaryRow in EventAttributeSummaryRows:
        print(EventAttributeSummaryRow.label, EventAttributeSummaryRow.records)


    # get all event attributes
    SQLstring = "SELECT "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.event_type_concept_id, "
    SQLstring += "       ha_event_attributes.event_attribute_id, "
    SQLstring += "       ha_event_attributes.event_attribute_type_concept_id, "
    SQLstring += "       ha_concepts.value_type_concept_id, "
    SQLstring += "       ha_event_attributes.value_text, "
    SQLstring += "       ha_event_attributes.value_numeric, "
    SQLstring += "       ha_event_attributes.value_datetime, "
    SQLstring += "       ha_concepts_1.code "
    SQLstring += "FROM   ((ha_events "
    SQLstring += "       INNER JOIN ha_event_attributes "
    SQLstring += "               ON ha_events.event_id = ha_event_attributes.event_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_event_attributes.event_attribute_type_concept_id = ha_concepts.concept_id) "
    SQLstring += "       LEFT JOIN ha_concepts AS ha_concepts_1 "
    SQLstring += "               ON ha_event_attributes.value_concept_id = ha_concepts_1.concept_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeRows = crsr.fetchall()

    # for EventAttributeRow in EventAttributeRows:
    #     print(EventAttributeRow.event_id)

    # write to CSV file

    destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"
    file_name = "rdv_demo"

    #Does file exist - if it does rename with time stamp
    file_ext = ".csv"

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    # writer = csv.writer(file, quoting=csv.QUOTE_NONNUMERIC)
    writer = csv.writer(file, quoting=csv.QUOTE_MINIMAL)
    # writer = csv.writer(file, quoting=csv.QUOTE_NONE, quotechar="", escapechar="?")

    crsr.execute(SQLstring)

    # Get column Headings

    out_row = []
    patient_col = []
    event_col = []
    out_row.append("event_id")

    for EventPatientAttributeSummaryRow in EventPatientAttributeSummaryRows:
        out_row.append(get_column_heading(EventPatientAttributeSummaryRow.label))
        patient_col.append(EventPatientAttributeSummaryRow.patient_attribute_type_concept_id)

    for EventAttributeSummaryRow in EventAttributeSummaryRows:
        out_row.append(get_column_heading(EventAttributeSummaryRow.label))
        event_col.append(EventAttributeSummaryRow.event_attribute_type_concept_id)

    writer.writerow(out_row)

    row_counter = 0
    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    for EventRow in EventRows:

        row_counter += 1

        sys.stdout.write("\r \r {0}".format(str(row_counter)))
        sys.stdout.flush()

        out_row = []
        out_row.append(EventRow.event_id)

        # For each event process list of patient_attributes
        column_pos = 0
        for EventPatientAttributeRow in EventPatientAttributeRows:
            if EventPatientAttributeRow.event_id == EventRow.event_id:
                # is this the next attribute?
                if patient_col[column_pos] == EventPatientAttributeRow.patient_attribute_type_concept_id:
                    out_row.append(EventPatientAttributeRow.patient_attribute_id)
                else:
                    out_row.append("NULL")
                column_pos += 1
        # may be the last attribute missing
        if column_pos <= len(patient_col)-1:
            for col in range(column_pos,len(patient_col)):
                out_row.append("NULL")

        # For each event process list of event_attributes
        column_pos = 0
        for EventAttributeRow in EventAttributeRows:
            if EventAttributeRow.event_id == EventRow.event_id:
                # If a row exists for an event it will always be output
                output_column = True
                while output_column:
                    # is this the next attribute?
                    if event_col[column_pos] == EventAttributeRow.event_attribute_type_concept_id:
                        # SQLstring += "       ha_event_attributes.value_text, "
                        # SQLstring += "       ha_event_attributes.value_numeric, "
                        # SQLstring += "       ha_event_attributes.value_datetime, "
                        # SQLstring += "       ha_concepts_1.code "

                        if EventAttributeRow.value_type_concept_id == 1:
                            out_row.append(EventAttributeRow.code)
                        elif EventAttributeRow.value_type_concept_id == 3:  # INTEGER
                            out_row.append(EventAttributeRow.value_numeric)
                        elif EventAttributeRow.value_type_concept_id == 4:  # FLOAT
                            out_row.append(EventAttributeRow.value_numeric)
                        elif EventAttributeRow.value_type_concept_id == 5:  # DATE
                            out_row.append(EventAttributeRow.value_datetime)
                        elif EventAttributeRow.value_type_concept_id == 6:  # TIME
                            out_row.append(EventAttributeRow.value_datetime)
                        elif EventAttributeRow.value_type_concept_id == 7:  # DATETIME
                            out_row.append(EventAttributeRow.value_datetime)
                        elif EventAttributeRow.value_type_concept_id == 8:  # TEXT
                            out_row.append(EventAttributeRow.value_text)
                        elif EventAttributeRow.value_type_concept_id == 9:  # BOOLEAN
                            out_row.append(EventAttributeRow.value_numeric)
                        else:
                            out_row.append(EventAttributeRow.event_attribute_id)
                        output_column = False
                    else:
                        out_row.append("NULL")
                    column_pos += 1
                    # This should be removed
                    if column_pos >= len(event_col):
                        break
            if column_pos >= len(event_col):
                break
        # may be the last attributes missing
        if column_pos <= len(event_col)-1:
            for col in range(column_pos,len(event_col)):
                out_row.append("NULL")

        writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")


def main():

    rep_conn_str = (
        r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
        r'DBQ=I:\DRE\Projects\Research\0004-Post mortem-AccessDB\DataExtraction\PMResearchReportDB.accdb;Uid=Admin;Pwd=;ExtendedAnsiSQL=1;'
    )
    rep_cnxn = pyodbc.connect(rep_conn_str)
    rep_crsr = rep_cnxn.cursor()

    create_rdv_complete(rep_cnxn, rep_crsr)

    # CreateHASCSVFiles(rep_cnxn, rep_crsr, destination_folder)

    rep_cnxn.close()


if __name__ == "__main__":
    main()

