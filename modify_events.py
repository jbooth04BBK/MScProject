import pyodbc
import pandas
import sys

import Create_HAS_Tables

def create_attribute_no_of_attributes(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''
    SQLstring = "SELECT "
    SQLstring += "  event_id, "
    SQLstring += "  COUNT(*) AS Attributes "
    SQLstring += "FROM ha_event_attributes "
    SQLstring += "GROUP BY "
    SQLstring += "  event_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeRows = crsr.fetchall()

    row = 0
    print("Processing Events - No Of Attributes")
    for EventAttributeRow in EventAttributeRows:

        row += 1
        sys.stdout.write("\r \r {0}".format(str(row)))
        sys.stdout.flush()

        Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventAttributeRow.event_id, "Observation/PostMortem", "ATTRIBUTES", "Number of attributes", "IN", EventAttributeRow.Attributes)

    print("")
    print("Done!")

def create_cod2_Summ_attribute_from_cod2_Attribute(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    'build an array of existing key values & labels from ha_concepts
    'Create an array of new attribute key values - add to ha_concepts
    'Assign new keys to old keys.
    'process all existing attributes
    '   add new event with mapped value

    ' code label
    ' 1  Unknown - No Abnormal Findings
    ' 2  Unknown - Non-Contributory Findings
    ' 3  Unknown - Possible/Probable Contributory Findings
    ' 4   Infection
    ' 5   CNS
    ' 6   Respiratory System
    ' 7   Cardiovascular System
    ' 8   GIT
    ' 9   Urogenital System
    '10  Lymphoreticular/Haematological
    '11  Musculoskeletal
    '12  Peripheral Nervous System/Neuromuscular Junction
    '13  Metabolic
    '14  Anaphylaxis
    '15  Immunological/Autoimmune
    '16  Neoplasia Benign
    '17  Neoplasia Malignant
    '18  Chromosomal Abnormality
    '19  Congenital Anomalies/Malformation Syndrome
    '20  Accidental
    '21  NAI/Homicide
    '22  Suicide
    '23  Wigglesworth 1
    '24  Wigglesworth 2
    '25  Wigglesworth 3
    '26  Wigglesworth 4
    '27  Wigglesworth 5
    '28  Normal Fetus
    '29  Traumatic NOS
    '30  Endocrine
    '32  No PM, For PM MRI ONLY
    '33  No PM, see Memo
    '34  Other
    '999 N/A

    'code label
    ' 1  Unknown (1 - 3)
    ' 2  Known   (4 - 30)
    ' 3  Other   (31 - 34)
    '999 N/A

    '''

    #Add Attributes and values
    #Not really necessary as will get added below
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    value_type_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/", None, "Concept")

    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "tblFinalDiagnoses", "tblFinalDiagnoses", value_type_concept_id)

    event_attribute_type_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", parent_concept_id, "COD2_SUMM", "COD2_SUMM", value_type_concept_id)

    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", None, "LookUp")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp", parent_concept_id, "COD2_SUMM", "COD2_SUMM", value_type_concept_id)

    value_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "001", "Unknown", value_type_concept_id)
    value_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "002", "known", value_type_concept_id)
    value_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "003", "Other", value_type_concept_id)
    value_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "994", "N/A", value_type_concept_id)

    #Get all Event Attributes of the original type

    #Get event_attributes for current event
    SQLstring = "SELECT "
    SQLstring += "  event_attribute_id, " # TODO we seem to have a duplicate CaseID
    SQLstring += "  event_id, "
    SQLstring += "  event_attribute_type_concept_id, "
    SQLstring += "  value_text, "
    SQLstring += "  value_numeric, "
    SQLstring += "  value_datetime, "
    SQLstring += "  value_concept_id, "
    SQLstring += "  value_type_concept_id, "
    SQLstring += "  code, "
    SQLstring += "  label "
    SQLstring += "FROM ha_event_attributes "
    SQLstring += "  LEFT OUTER JOIN ha_concepts "
    SQLstring += "    ON ha_concepts.concept_id = ha_event_attributes.value_concept_id "
    SQLstring += "WHERE "
    SQLstring += "  (ha_concepts.category = '/EventAttribute/Observation/PostMortem/LookUp/COD2_COD2ID') "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeRows = crsr.fetchall()

    row = 0
    print("Processing Events - COD2_SUMM Attribute From COD2 Attribute")

    for EventAttributeRow in EventAttributeRows:

        row += 1
        sys.stdout.write("\r \r {0}".format(str(row)))
        sys.stdout.flush()

        if EventAttributeRow.code in ("001", "002", "003", "999"):
            code = "001"
            text = "Unknown"
        elif EventAttributeRow.code in ("031", "032", "033", "034"):
            code = "003"
            text = "Other"
        else:
           code = "002"
           text = "Known"

        # print(row, EventAttributeRow.code, "Observation/PostMortem/tblFinalDiagnoses", "COD2_SUMM", "COD2_SUMM", "ID", code, text)
        Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventAttributeRow.event_id, "Observation/PostMortem/tblFinalDiagnoses", "COD2_SUMM", "COD2_SUMM", "ID", code, text)

    print("")
    print("Done!")


def create_reporting_attributes(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''

    # Make sure that PostMortem/Reporting exists - ToDo move to create_core_concepts
    value_type_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/", None, "Concept")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "Reporting", "Reporting", value_type_concept_id)

    #Get all events
    SQLstring = "SELECT "
    SQLstring += "  event_id, "
    SQLstring += "  patient_id, "
    SQLstring += "  start_date "
    SQLstring += "FROM "
    SQLstring += "  ha_events AS EV "
    SQLstring += "  INNER JOIN "
    SQLstring += "    ha_concepts AS CO "
    SQLstring += "      ON EV.event_type_concept_id = CO.concept_id "
    SQLstring += "WHERE "
    SQLstring += "  CO.category = '/Event/Observation' "
    SQLstring += "  AND CO.code = 'PostMortem' "
    SQLstring += ";"

    crsr.execute(SQLstring)

    EventRows = crsr.fetchall()

    row = 0
    start_row = 0

    print("Processing Events - Adding Reporting Attributes")

    for EventRow in EventRows:

        row += 1
        print(row, EventRow.patient_id)

        if row >= start_row:

            #Defined as having a External exam if Body weight is greater than 0
            category = r"/EventAttribute/Observation/PostMortem/tblExternalExams"
            code = r"BodyWeight"

            event_attribute_id = Create_HAS_Tables.GetEventAttributeID(cnxn, crsr, EventRow.event_id, category, code)

            if event_attribute_id != None:
                Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "ExternalExam", "External Examination", "TF", 1)
            else:
                Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "ExternalExam", "External Examination", "TF", 0)

            # Defined as having a Internal exam if Heart weight is greater than 0
            category = r"/EventAttribute/Observation/PostMortem/tblInternalExams"
            code = r"HeartWeight"

            event_attribute_id = Create_HAS_Tables.GetEventAttributeID(cnxn, crsr, EventRow.event_id, category, code)

            if event_attribute_id != None:
                Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "InternalExam", "Internal Examination", "TF", 1)
            else:
                Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "InternalExam", "Internal Examination", "TF", 0)

            # Get number of samples taken
            category = r"/Event/Observation"
            code = r"LAB_EPISODE"

            events = Create_HAS_Tables.CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code)

            if events > 0:
                #Get number of samples
                Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "SAMPLETKN", "Samples Taken", "IN", events)

                # Defined as having Microbiology if any Test Sets B*
                category = r"/Event/Observation"
                code = r"LT"
                value_code = r"B"

                events = Create_HAS_Tables.CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code, value_code)

                if events != None:
                    Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "MICRO", "Microbiology Tests", "TF", 1)
                else:
                    Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "MICRO", "Microbiology Tests", "TF", 0)

                # Defined as having Microbiology if any Test Sets B*
                category = r"/Event/Observation"
                code = r"LT"
                value_code = r"V"

                events = Create_HAS_Tables.CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code, value_code)

                if events != None:
                    Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "VIROLOGY", "Virology Tests", "TF", 1)
                else:
                    Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "VIROLOGY", "Virology Tests", "TF", 0)

    print("")
    print("Done!")


def check_measurements(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''

    #Get all events
    SQLstring = "SELECT "
    SQLstring += "  event_id, "
    SQLstring += "  patient_id, "
    SQLstring += "  start_date "
    SQLstring += "FROM "
    SQLstring += "  ha_events AS EV "
    SQLstring += "  INNER JOIN "
    SQLstring += "    ha_concepts AS CO "
    SQLstring += "      ON EV.event_type_concept_id = CO.concept_id "
    SQLstring += "WHERE "
    SQLstring += "  CO.category = '/Event/Observation' "
    SQLstring += "  AND CO.code = 'PostMortem' "
    SQLstring += ";"

    crsr.execute(SQLstring)

    EventRows = crsr.fetchall()

    row = 0
    start_row = 0

    print("Processing Events - Checking Measurements")

    for EventRow in EventRows:

        row += 1
        print(row, EventRow.patient_id)

        if row >= start_row:

            #Defined as having a External exam if Body weight is greater than 0
            category = r"/EventAttribute/Observation/PostMortem/tblExternalExams"
            code = r"BodyWeight"

            event_attribute_id = Create_HAS_Tables.GetEventAttributeID(cnxn, crsr, EventRow.event_id, category, code)

            if event_attribute_id != None:
                pass

    print("")
    print("Done!")

def create_attribute_inc_in_study(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''
    # Make sure that PostMortem/INC_IN_STUDY exists - ToDo move to create_core_concepts
    value_type_concept_id = Create_HAS_Tables.GetValueTypeConceptId(cnxn, crsr, "ID")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "INC_IN_STUDY", "Include in study", value_type_concept_id)
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", None, "LookUp")
    parent_concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp", parent_concept_id, "INC_IN_STUDY", "Include in study", value_type_concept_id)
    concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/INC_IN_STUDY", parent_concept_id, "001", "Include in study", value_type_concept_id)
    concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/INC_IN_STUDY", parent_concept_id, "002", "Exclude - COD", value_type_concept_id)
    concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/INC_IN_STUDY", parent_concept_id, "003", "Exclude - Age", value_type_concept_id)
    concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/INC_IN_STUDY", parent_concept_id, "004", "Exclude - Incorrect Measurement", value_type_concept_id)
    concept_id = Create_HAS_Tables.GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/INC_IN_STUDY", parent_concept_id, "005", "Exclude - Missing value", value_type_concept_id)

    # Get all events
    SQLstring = "SELECT "
    SQLstring += "  event_id "
    SQLstring += "FROM ha_event_attributes "
    SQLstring += "GROUP BY "
    SQLstring += "  event_id "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeRows = crsr.fetchall()

    row = 0
    inserted = 0
    updated = 0
    print("Processing Events - Inc in study")
    for EventAttributeRow in EventAttributeRows:

        row += 1
        # get value if exists
        value = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id, "/EventAttribute/Observation/PostMortem", "INC_IN_STUDY")
        if pandas.isnull(value):
            Create_HAS_Tables.AddEventAttribute(cnxn, crsr, EventAttributeRow.event_id, "Observation/PostMortem", "INC_IN_STUDY", "Include in study", "ID", "001")
            inserted += 1
        elif value != 1:
            caseid = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id,"/EventAttribute/Observation/PostMortem/tblCases","CASEID")
            Create_HAS_Tables.update_event_attribute_value(cnxn, crsr, caseid,"/EventAttribute/Observation/PostMortem", "INC_IN_STUDY", "001")
            updated += 1

        sys.stdout.write("\r \r {0}, {1}, {2}".format(str(row), str(inserted), str(updated)))
        sys.stdout.flush()

    print("")
    print("Done!")

def exclude_event_attributes(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:

    1. Check all inc_in_study set to T
    2. Exclude on basis of age_category and if age_category = 006 then age_in_days > 730
    3. Exclude out layer measurements
    4.

    '''

    # create_attribute_inc_in_study(cnxn, crsr)

    # Get all events
    SQLstring = "SELECT "
    SQLstring += "  event_id, "
    SQLstring += "  patient_id "
    SQLstring += "FROM ha_events "
    SQLstring += ";"

    crsr.execute(SQLstring)
    EventAttributeRows = crsr.fetchall()

    row = 0
    excluded = 0
    print("Processing Events - Exclude from study")
    for EventAttributeRow in EventAttributeRows:

        row += 1
        # get value if exists
        value = Create_HAS_Tables.get_patient_attribute_value(cnxn, crsr, EventAttributeRow.patient_id, "/PatientAttribute", "AC")
        if not pandas.isnull(value):
            if value == "001":
                caseid = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id,"/EventAttribute/Observation/PostMortem/tblCases","CASEID")
                Create_HAS_Tables.update_event_attribute_value(cnxn, crsr, caseid,"/EventAttribute/Observation/PostMortem", "INC_IN_STUDY", 0)
                excluded += 1
            elif value == "002":
                caseid = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id,"/EventAttribute/Observation/PostMortem/tblCases","CASEID")
                Create_HAS_Tables.update_event_attribute_value(cnxn, crsr, caseid,"/EventAttribute/Observation/PostMortem", "INC_IN_STUDY",0)
                excluded += 1
            elif value == "006":
                age_in_days = Create_HAS_Tables.get_patient_attribute_value(cnxn, crsr, EventAttributeRow.patient_id,"/PatientAttribute", "AG")
                if pandas.isnull(age_in_days):
                    caseid = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id, "/EventAttribute/Observation/PostMortem/tblCases", "CASEID")
                    Create_HAS_Tables.update_event_attribute_value(cnxn, crsr, caseid,"/EventAttribute/Observation/PostMortem", "INC_IN_STUDY", 0)
                    excluded += 1
                elif age_in_days > 730:
                    caseid = Create_HAS_Tables.get_event_attribute_value(cnxn, crsr, EventAttributeRow.event_id, "/EventAttribute/Observation/PostMortem/tblCases", "CASEID")
                    Create_HAS_Tables.update_event_attribute_value(cnxn, crsr, caseid,"/EventAttribute/Observation/PostMortem", "INC_IN_STUDY", 0)
                    excluded += 1

        sys.stdout.write("\r \r {0}, {1}".format(str(row), str(excluded)))
        sys.stdout.flush()

    print("")
    print("Done!")
    pass

def main():

    rep_conn_str = (
        r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
        r'DBQ=I:\DRE\Projects\Research\0004-Post mortem-AccessDB\DataExtraction\PMResearchReportDB.accdb;Uid=Admin;Pwd=;ExtendedAnsiSQL=1;'
    )
    rep_cnxn = pyodbc.connect(rep_conn_str)
    rep_crsr = rep_cnxn.cursor()

    res_conn_str = (
        r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
        r'DBQ=I:\DRE\Projects\Research\0004-Post mortem-AccessDB\DataExtraction\December17Master.accdb;Uid=Admin;Pwd=FSID2005$;ExtendedAnsiSQL=1;'
    )
    res_cnxn = pyodbc.connect(res_conn_str)
    res_crsr = res_cnxn.cursor()

    # create_cod2_Summ_attribute_from_cod2_Attribute(rep_cnxn, rep_crsr)

    # only for Post Mortem Events
    # create_attribute_no_of_attributes(rep_cnxn, rep_crsr)

    # create_reporting_attributes(rep_cnxn, rep_crsr)

    create_attribute_inc_in_study(rep_cnxn, rep_crsr)

    # exclude_event_attributes(rep_cnxn, rep_crsr)

    rep_cnxn.close()
    res_cnxn.close()


if __name__ == "__main__":
    main()

