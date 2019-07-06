import pyodbc
import pandas
import datetime
import decimal
import sys
import time
import csv
import os
import re

import Create_HAS_Tables

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

# def file_time_stamp():
#     return str(datetime.datetime.now())[:19].replace("-", "").replace(":", "").replace(" ", "_")

def format_csv_output(value_type_concept_id, code, value_numeric, value_datetime, value_text):

    if value_type_concept_id == 1:
        output_text = "C" + code
    elif value_type_concept_id == 3:  # INTEGER
        output_text = "{:.0f}".format(value_numeric)
    elif value_type_concept_id == 4:  # FLOAT
        output_text = "{:.4f}".format(value_numeric)
    elif value_type_concept_id == 5:  # DATE
        output_text = '{0:%Y-%m-%d}'.format(value_datetime)
    elif value_type_concept_id == 6:  # TIME
        output_text = '{0:%H:%M:%S}'.format(value_datetime)
    elif value_type_concept_id == 7:  # DATETIME
        output_text = '{0:%Y-%m-%d %H:%M:%S}'.format(value_datetime)
    elif value_type_concept_id == 8:  # TEXT
        output_text = value_text
    elif value_type_concept_id == 9:  # BOOLEAN
        if value_numeric == 0:
            output_text = "F"
        else:
            output_text = "T"
    else:
        output_text = "Unknown Value Type: {:.0f}".format(value_type_concept_id)

    return output_text

def getAllEventRows(cnxn, crsr):

    SQLstring = "SELECT"
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date "
    SQLstring += "FROM   ha_events "
    SQLstring += "ORDER BY "
    SQLstring += "       ha_events.event_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    return crsr.fetchall()

def getEventRows(cnxn, crsr, EventPatientAttributes = [], EventAttributes = []):

    # get patient attributes values

    SQLstring = "SELECT"
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date, "
    SQLstring += "       ha_patients.sex "
    SQLstring += "FROM   (ha_events "
    SQLstring += "       INNER JOIN ha_patients "
    SQLstring += "               ON ha_events.patient_id = ha_patients.patient_id) "
    SQLstring += "       INNER JOIN ha_patient_attributes "
    SQLstring += "               ON ha_patients.patient_id = ha_patient_attributes.patient_id "

    if len(EventPatientAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id IN (" + ','.join(map(str, EventPatientAttributes)) + ") "

    SQLstring += "GROUP BY "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date, "
    SQLstring += "       ha_patients.sex "

    SQLstring += "UNION "

    # get all event attributes
    SQLstring += "SELECT "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date, "
    SQLstring += "       ha_patients.sex "
    SQLstring += "FROM   (ha_events "
    SQLstring += "       INNER JOIN ha_patients "
    SQLstring += "               ON ha_events.patient_id = ha_patients.patient_id) "
    SQLstring += "       INNER JOIN ha_event_attributes "
    SQLstring += "               ON ha_events.event_id = ha_event_attributes.event_id "

    if len(EventAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_event_attributes.event_attribute_type_concept_id IN (" + ','.join(map(str, EventAttributes)) + ") "

    SQLstring += "GROUP BY "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date, "
    SQLstring += "       ha_patients.sex "
    SQLstring += "ORDER BY "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_events.start_date "
    SQLstring += ";"

    crsr.execute(SQLstring)
    return crsr.fetchall()



def getEventPatientAttributeSummaryRows(cnxn, crsr, EventPatientAttributes = [], EventPatientAttributeFilters = []):

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

    if len(EventPatientAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id IN (" + ','.join(map(str, list(set(EventPatientAttributes + EventPatientAttributeFilters)))) + ") "

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
    return crsr.fetchall()


def getEventPatientAttributeRows(cnxn, crsr, EventPatientAttributes = [], EventPatientAttributeFilters = []):

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
    SQLstring += "       ha_patient_attributes.value_text, "
    SQLstring += "       ha_patient_attributes.value_numeric, "
    SQLstring += "       ha_patient_attributes.value_datetime, "
    SQLstring += "       ha_patient_attributes.value_concept_id, "
    SQLstring += "       ha_concepts_1.code "
    SQLstring += "FROM   (((ha_events "
    SQLstring += "       INNER JOIN ha_patients "
    SQLstring += "               ON ha_events.patient_id = ha_patients.patient_id) "
    SQLstring += "       INNER JOIN ha_patient_attributes "
    SQLstring += "               ON ha_patients.patient_id = ha_patient_attributes.patient_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_patient_attributes.patient_attribute_type_concept_id = ha_concepts.concept_id) "
    SQLstring += "       LEFT JOIN ha_concepts AS ha_concepts_1 "
    SQLstring += "               ON ha_patient_attributes.value_concept_id = ha_concepts_1.concept_id "

    if len(EventPatientAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id IN (" + ','.join(map(str, list(set(EventPatientAttributes + EventPatientAttributeFilters)))) + ") "

    SQLstring += "ORDER BY "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_patient_attributes.patient_attribute_type_concept_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    return crsr.fetchall()

def getEventAttributeSummaryRows(cnxn, crsr, EventAttributes = [], EventAttributeFilters = []):

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

    if len(EventAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_event_attributes.event_attribute_type_concept_id IN (" + ','.join(map(str, list(set(EventAttributes + EventAttributeFilters)))) + ") "

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
    return crsr.fetchall()

def getEventAttributeRows(cnxn, crsr, EventAttributes = [], EventAttributeFilters = []):

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
    SQLstring += "       ha_event_attributes.value_concept_id, "
    SQLstring += "       ha_concepts_1.code "
    SQLstring += "FROM   ((ha_events "
    SQLstring += "       INNER JOIN ha_event_attributes "
    SQLstring += "               ON ha_events.event_id = ha_event_attributes.event_id) "
    SQLstring += "       INNER JOIN ha_concepts "
    SQLstring += "               ON ha_event_attributes.event_attribute_type_concept_id = ha_concepts.concept_id) "
    SQLstring += "       LEFT JOIN ha_concepts AS ha_concepts_1 "
    SQLstring += "               ON ha_event_attributes.value_concept_id = ha_concepts_1.concept_id "

    if len(EventAttributes) > 0:
        SQLstring += "WHERE "
        SQLstring += "       ha_event_attributes.event_attribute_type_concept_id IN (" + ','.join(map(str, list(set(EventAttributes + EventAttributeFilters)))) + ") "

    SQLstring += "ORDER BY "
    SQLstring += "       ha_events.event_id, "
    SQLstring += "       ha_event_attributes.event_attribute_type_concept_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    return crsr.fetchall()

def create_rdv_selection(cnxn, crsr):

    # Select Patient Attributes
    EventPatientAttributes = []
    EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "GA")) # Gestational Age

    # Select Patient Attribute Filters
    EventPatientAttributeFilters = []
    EventPatientAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AC")) # 44 = Age Category
    EventPatientAttributeFilterValues = []
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "001"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "002"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "003"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "004"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "005"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "006"))

    # Select Event Attributes
    EventAttributes = []
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "CASEID"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "Year")) # Year
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "SSN")) # Season
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None, "COD2_SUMM"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", None, "ATTRIBUTES"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None, "ExternalExam"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None, "InternalExam"))

    # Select Event Attribute Filters
    EventAttributeFilters = []
    EventAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None,"COD2_SUMM"))
    EventAttributeFilterValues = []
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None,"001"))
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None,"002"))

    file_name = "rdv_demo_selection_02"

    create_rdv(cnxn, crsr, file_name, EventPatientAttributes, EventPatientAttributeFilters, EventPatientAttributeFilterValues, EventAttributes, EventAttributeFilters, EventAttributeFilterValues)


def create_rdv_ac_measurements(cnxn, crsr):

    age_categories = ["001","002","003","004","005","006"]

    for age_category in age_categories:

        # Select Patient Attributes
        EventPatientAttributes = []
        if age_category not in ["001","002"]:
            EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AG")) # Age in Days
        EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "GA")) # Gestation At Delivery In Days

        # Select Patient Attribute Filters
        EventPatientAttributeFilters = []
        EventPatientAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AC")) # Age Category
        EventPatientAttributeFilterValues = []
        EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, age_category)) # 44 = Age Category

        # Select Event Attributes
        EventAttributes = []
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None,"CASEID"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None,"ExternalExam"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None,"InternalExam"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "BodyWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "CrownRumpLength"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "HeadCircumference"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "CrownRumpLength"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "BodyLength"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "FootLength"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "LeftFootLength"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "RightFootLength"))

        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "HeartWeight"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "RightLungWeight"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LeftLungWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombLungWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LiverWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "PancreasWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "ThymusWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "SpleenWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombAdrenalWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "ThyroidWeight"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "RightKidneyWeight"))
        # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LeftKidneyWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombKidneyWeight"))
        EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "BrainWeight"))

        # Is this necessary if measurements were made we could use them.
        EventAttributeFilters = []
        EventAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None,"COD2_SUMM"))
        EventAttributeFilterValues = []
        EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "001"))
        EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "002"))

        file_name = "rdv_" + age_category + "_ext_measurements"

        create_rdv(cnxn, crsr, file_name, EventPatientAttributes, EventPatientAttributeFilters, EventPatientAttributeFilterValues, EventAttributes, EventAttributeFilters, EventAttributeFilterValues)


def create_rdv_measurements(cnxn, crsr):

    # Select Patient Attributes
    EventPatientAttributes = []
    EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AG")) # Age in Days
    EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "GA")) # Gestation At Delivery In Days

    # Select Patient Attribute Filters
    EventPatientAttributeFilters = []
    EventPatientAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AC")) # Age Category
    EventPatientAttributeFilterValues = []
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "001"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "002"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "003"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "004"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "005"))
    EventPatientAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute/AC", None, "006"))

    # Select Event Attributes
    EventAttributes = []
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "CASEID"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "BodyWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "CrownRumpLength"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "HeadCircumference"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "CrownRumpLength"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "BodyLength"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "FootLength"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "LeftFootLength"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblExternalExams", None, "RightFootLength"))

    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "HeartWeight"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "RightLungWeight"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LeftLungWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombLungWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LiverWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "PancreasWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "ThymusWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "SpleenWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombAdrenalWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "ThyroidWeight"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "RightKidneyWeight"))
    # EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "LeftKidneyWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "CombKidneyWeight"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblInternalExams", None, "BrainWeight"))

    # Is this necessary if measurements were made we could use them.
    EventAttributeFilters = []
    EventAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None,"COD2_SUMM"))
    EventAttributeFilterValues = []
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "001"))
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "002"))

    file_name = "rdv_" + "measurements"

    create_rdv(cnxn, crsr, file_name, EventPatientAttributes, EventPatientAttributeFilters, EventPatientAttributeFilterValues, EventAttributes, EventAttributeFilters, EventAttributeFilterValues)


def create_rdv_new_attributes(cnxn, crsr):

    # Select Patient Attributes
    EventPatientAttributes = []
    EventPatientAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/PatientAttribute", None, "AC")) # Age Category

    # Select Patient Attribute Filters
    EventPatientAttributeFilters = []
    EventPatientAttributeFilterValues = []

    # Select Event Attributes
    EventAttributes = []
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "CASEID"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "Year")) # Year
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblCases", None, "SSN")) # Season
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None, "COD2_SUMM"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", None, "ATTRIBUTES"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None, "ExternalExam"))
    EventAttributes.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/Reporting", None, "InternalExam"))

    # Select Event Attribute Filters
    EventAttributeFilters = []
    EventAttributeFilters.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", None, "COD2_SUMM"))
    EventAttributeFilterValues = []
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "001"))
    EventAttributeFilterValues.append(Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", None, "002"))

    file_name = "rdv_new_attributes"

    create_rdv(cnxn, crsr, file_name, EventPatientAttributes, EventPatientAttributeFilters, EventPatientAttributeFilterValues, EventAttributes, EventAttributeFilters, EventAttributeFilterValues)


def create_rdv_complete(cnxn, crsr):

    file_name = "rdv_demo_complete"

    create_rdv(cnxn, crsr, file_name)

def create_rdv(cnxn, crsr, file_name, EventPatientAttributes = [], EventPatientAttributeFilters = [], EventPatientAttributeFilterValues = [], EventAttributes = [], EventAttributeFilters = [], EventAttributeFilterValues = []):

    '''

    :param cnxn: Connection to database
    :param crsr: Cursor for all SQL operations

    '''

    # Get rows

    # Create selections
    EventPatientAttributeSummaryRows = getEventPatientAttributeSummaryRows(cnxn, crsr, EventPatientAttributes, EventPatientAttributeFilters)

    EventPatientAttributeRows = getEventPatientAttributeRows(cnxn, crsr, EventPatientAttributes, EventPatientAttributeFilters)

    EventAttributeSummaryRows = getEventAttributeSummaryRows(cnxn, crsr, EventAttributes, EventAttributeFilters)

    EventAttributeRows = getEventAttributeRows(cnxn, crsr, EventAttributes, EventAttributeFilters)

    # Get list of event_ids
    EventRows = getEventRows(cnxn, crsr, EventPatientAttributes, EventAttributes)


    # write to CSV file

    destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"
    file_ext = ".csv"

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + Create_HAS_Tables.file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')
    writer = csv.writer(file, quoting=csv.QUOTE_MINIMAL)

    # Get column Headings

    out_row = []
    patient_col = []
    event_col = []
    xml_row = []
    out_row.append("event_id")
    xml_row.append(("event_id",3)) # Integer
    out_row.append("event_start_date")
    xml_row.append(("event_start_date",7)) # datetime
    out_row.append("sex")
    xml_row.append(("sex",8)) # datetime

    for EventPatientAttributeSummaryRow in EventPatientAttributeSummaryRows:
        column_name = get_column_heading(EventPatientAttributeSummaryRow.label)
        out_row.append(column_name)
        patient_col.append(EventPatientAttributeSummaryRow.patient_attribute_type_concept_id)
        xml_row.append((column_name,EventPatientAttributeSummaryRow.value_type_concept_id))

    for EventAttributeSummaryRow in EventAttributeSummaryRows:
        column_name = get_column_heading(EventAttributeSummaryRow.label)
        out_row.append(column_name)
        event_col.append(EventAttributeSummaryRow.event_attribute_type_concept_id)
        xml_row.append((column_name,EventAttributeSummaryRow.value_type_concept_id))

    writer.writerow(out_row)

    row_counter = 0
    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    for EventRow in EventRows:

        row_counter += 1

        sys.stdout.write("\r \r {0}".format(str(row_counter)))
        sys.stdout.flush()

        # Check filters
        include_event = True
        if len(EventPatientAttributeFilterValues) > 0:
            include_event = False
            for EventPatientAttributeRow in EventPatientAttributeRows:
                if EventPatientAttributeRow.event_id == EventRow.event_id:
                    if EventPatientAttributeRow.value_concept_id in EventPatientAttributeFilterValues:
                        include_event = True

        # If already exclude by patient attributes can't add back in - This could be an option for OR
        if include_event and len(EventAttributeFilterValues) > 0:
            include_event = False
            for EventAttributeRow in EventAttributeRows:
                if EventAttributeRow.event_id == EventRow.event_id:
                    if EventAttributeRow.value_concept_id in EventAttributeFilterValues:
                        include_event = True

        if include_event:
            out_row = []
            out_row.append(EventRow.event_id)
            out_row.append('{0:%Y-%m-%d %H:%M:%S}'.format(EventRow.start_date))
            if not pandas.isnull(EventRow.sex):
                out_row.append(EventRow.sex.upper())
            else:
                out_row.append("U")

            # For each event process list of patient_attributes
            column_pos = 0
            for EventPatientAttributeRow in EventPatientAttributeRows:
                if EventPatientAttributeRow.event_id == EventRow.event_id:
                    # If a row exists for an event it will always be output
                    output_column = True
                    while output_column:
                        # is this the next attribute?
                        if patient_col[column_pos] == EventPatientAttributeRow.patient_attribute_type_concept_id:
                            output_text = format_csv_output(EventPatientAttributeRow.value_type_concept_id,
                                                            EventPatientAttributeRow.code,
                                                            EventPatientAttributeRow.value_numeric,
                                                            EventPatientAttributeRow.value_datetime,
                                                            EventPatientAttributeRow.value_text)

                            out_row.append(output_text)


                            output_column = False

                        else:
                            out_row.append("") # NULL
                        column_pos += 1
            # may be the last attribute missing
            if column_pos <= len(patient_col)-1:
                for col in range(column_pos,len(patient_col)):
                    out_row.append("") # NULL

            # For each event process list of event_attributes
            column_pos = 0
            for EventAttributeRow in EventAttributeRows:
                if EventAttributeRow.event_id == EventRow.event_id:
                    # If a row exists for an event it will always be output
                    output_column = True
                    while output_column:
                        # is this the next attribute?
                        if event_col[column_pos] == EventAttributeRow.event_attribute_type_concept_id:

                            output_text = format_csv_output(EventAttributeRow.value_type_concept_id,
                                                            EventAttributeRow.code,
                                                            EventAttributeRow.value_numeric,
                                                            EventAttributeRow.value_datetime,
                                                            EventAttributeRow.value_text)

                            out_row.append(output_text)

                            output_column = False

                        else:
                            out_row.append("") # NULL
                        column_pos += 1
            # may be the last attributes missing
            if column_pos <= len(event_col)-1:
                for col in range(column_pos,len(event_col)):
                    out_row.append("") # NULL

            writer.writerow(out_row)

    file.close()
    print("")
    print("file closed")

    #Create XML file

    XMLLine =  '<?xml version="1.0" encoding="UTF-8"?>'
    XMLLine += '<DatasetDefinition xmlns="http://aridhia-mgrid.com/ddf/2" TableName="' + file_name + '" Action="create">'
    XMLLine += '<Title>' + file_name + '</Title>'
    XMLLine += '<Format Delimiter="," TextQualifier="&quot;" Encoding="UTF-8" HeaderCase="leave alone" Header="true" CopyToFile="true" NullQualifier="NULL"/>'
    XMLLine += '<Columns>'

    for column in xml_row:

        column_name = column[0]
        value_type_concept_id = column[1]

        if value_type_concept_id == 1:
            column_type = "text"
        elif value_type_concept_id == 3:  # INTEGER
            column_type = "integer"
        elif value_type_concept_id == 4:  # FLOAT
            column_type = "double precision"
        elif value_type_concept_id == 5:  # DATE
            column_type = "date"
        elif value_type_concept_id == 6:  # TIME
            column_type = "timestamp"
        elif value_type_concept_id == 7:  # DATETIME
            column_type = "timestamp"
        elif value_type_concept_id == 8:  # TEXT
            column_type = "text"
        elif value_type_concept_id == 9:  # BOOLEAN
            column_type = "text"
        else:
            column_type = "error type: " + str(column[1])

        if ":FIRST_NAME:MIDDLE_NAMES:SURNAME:BIRTH_DATE:DEATH_DATE:POSTCODE:".find((":" + column_name + ":").upper()) >= 0:
            column_deid_type = "drop"
        elif ":HEALTH_IDENTIFIER:ALT_HEALTH_IDENTIFIER:STAFF_CODE:HOSPITAL_NO:".find((":" + column_name + ":").upper()) >= 0:
            column_deid_type = "pseudonymize"
        else:
            column_deid_type = "keep"

        # print(column_name, column_type, column_deid_type)

        XMLLine += '<Column Name="' + column_name + '" Type="' + column_type + '" Deidentify="' + column_deid_type + '"/>'

    XMLLine += '</Columns>'
    XMLLine += '</DatasetDefinition>'

    #Does file exist - if it does rename with time stamp
    file_ext = ".xml"

    # If file left over from last run - rename it, so start fresh.
    if os.path.isfile(destination_folder + file_name + file_ext):
        new_fname = file_name + "_" + Create_HAS_Tables.file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')

    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    file.write(XMLLine)

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

    # create_rdv_complete(rep_cnxn, rep_crsr)

    # create_rdv_new_attributes(rep_cnxn, rep_crsr)

    # create_rdv_selection(rep_cnxn, rep_crsr)

    create_rdv_measurements(rep_cnxn, rep_crsr)

    rep_cnxn.close()


if __name__ == "__main__":
    main()

