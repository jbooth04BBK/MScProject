import pyodbc
import pandas
import datetime
import decimal
import sys
import time
import csv
import os

def create_rdv_complete(cnxn, crsr):

    '''

    :param cnxn: Connection to database
    :param crsr: Cursor for all SQL operations

    '''

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

    for EventPatientAttributeRow in EventPatientAttributeRows:
        print(EventPatientAttributeRow.event_id)

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

    for EventAttributeRow in EventAttributeRows:
        print(EventAttributeRow.event_id)


def main():

    rep_conn_str = (
        r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
        r'DBQ=I:\DRE\Projects\Research\0004-Post mortem-AccessDB\DataExtraction\PMResearchReportDB.accdb;Uid=Admin;Pwd=;ExtendedAnsiSQL=1;'
    )
    rep_cnxn = pyodbc.connect(rep_conn_str)
    rep_crsr = rep_cnxn.cursor()

    create_rdv_complete(rep_cnxn, rep_crsr)

    destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"

    # CreateHASCSVFiles(rep_cnxn, rep_crsr, destination_folder)

    rep_cnxn.close()


if __name__ == "__main__":
    main()

