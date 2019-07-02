import pyodbc
import pandas
import datetime
import decimal
import sys
import time
import csv
import os

gbl_add_profiling = False
gbl_add_event_time = 0.0
gbl_add_att_time = 0.0

def file_time_stamp():
    return str(datetime.datetime.now())[:19].replace("-", "").replace(":", "").replace(" ", "_")

def isDate(date_text):
    try:
        datetime.datetime.strptime(date_text, '%Y-%m-%d %H:%M')
        return True
    except ValueError:
        return False
    except:
        return False

def Nz(value_test, value_return):
    if pandas.isnull(value_test):
        return value_return
    else:
        return value_test

def GetAgeCategory(GestationInDays, AgeInDays, ReferralText = None):
    '''
    Returns an age category based on gestation of age both in days
    If neither of the values are available it derives the age category from
    the referral category of the case, on the premise that those who refer the case know the age and
    that referral categoriws and age categories are readily mapable.
    '''
    #Deal with NULL values
    if pandas.isnull(GestationInDays):
        GestationInDays = 0
    if pandas.isnull(AgeInDays):
        AgeInDays = 0
    if pandas.isnull(ReferralText):
        ReferralText = ""

    if GestationInDays <= 0 and AgeInDays <= 0:
        # If strReferral text is given try and match to Age category
        if len(ReferralText) > 0:
            if ReferralText == "Sudden Death < 12 months":
                return "Infant Death"
            elif  ReferralText == "Sudden Death > 12 months":
                return "Child Death"
            elif ReferralText == "Miscarriage Late":
                return "Miscarriage"
            elif ReferralText == "Stillbirth":
                return "Still Birth"
            elif ReferralText == "Neonatal Death":
                return "Neonatal"
            elif ReferralText.upper() == "TOP":
                return "Miscarriage"
            elif ReferralText == "Miscarriage Early":
                return "Miscarriage"
            else:
                return "N/A"
        else:
            return "N/A"
    else:
        if GestationInDays > 0 and AgeInDays == 0:
            if GestationInDays < (24 * 7):
                return "Miscarriage"
            else:
                return "Still Birth"
        else:
            if AgeInDays <= 7:
                return "Early Neonatal"
            elif AgeInDays <= 28:
                return "Neonatal"
            elif AgeInDays <= 365:
                return "Infant Death"
            else:
                return "Child Death"

def GetAgeCategoryID(GestationInDays, AgeInDays, ReferralText = None):
    '''
    returns an integer for sorting Age category chronologically
    based on string returned by modQueries_AgeCategory
    '''
    #Deal with NULL values
    if pandas.isnull(GestationInDays):
        GestationInDays = 0
    if pandas.isnull(AgeInDays):
        AgeInDays = 0
    if pandas.isnull(ReferralText):
        ReferralText = ""

    AgeCategory = GetAgeCategory(GestationInDays, AgeInDays, ReferralText)

    if AgeCategory == "Miscarriage":
        return 1
    elif AgeCategory == "Still Birth":
        return 2
    elif AgeCategory == "Early Neonatal":
        return 3
    elif AgeCategory == "Neonatal":
        return 4
    elif AgeCategory == "Infant Death":
        return 5
    elif AgeCategory == "Child Death":
        return 6
    else:
        return 999

def GetPMYear(PMNumber):
    '''
    Normally format for PMNumber = '99P000' where '99' represents the last 2 digits of the year
    12P234 - year = 2012
    '''
    if pandas.isnull(PMNumber):
        return 0
    # PM Number should always be 6 charcters
    if len(PMNumber) < 6:
        return 0
    else:
        #check if one or other or both of the first 2 characters is not a number
        if not PMNumber[:2].isnumeric():
            return 0
        else:
            Year = int(PMNumber[:2])
            #There are some years before the year 2000
            if Year > 90:
                return 1900 + Year
            #To cope with Julie's fudge! PM Numbers from another hospital had their years reversed, only
            # 2012 and 2013 so are 21 and 31.
            elif Year > 20:
                return 2000 + int(PMNumber[1:2] + PMNumber[:1])
            else:
                return 2000 + Year

#TODO rename return_null_number as sql_number, etc

def return_null_number(variable):
    if pandas.isnull(variable):
        return "NULL"
    else:
        return str(variable)

def return_null_string(variable):
    if pandas.isnull(variable):
        return "NULL"
    else:
        return "'" + variable.replace("'","''") + "'"

def return_null_date(variable):
    if pandas.isnull(variable):
        return "NULL"
    else:
        return "#" + str(variable)[:10] + "#"

def return_null_datetime(variable):
    if pandas.isnull(variable):
        return "NULL"
    else:
        return "#" + str(variable)[:19] + "#"

def drop_table(cnxn, crsr, table_name):
    '''
    Checks if a table exists within the connected database
    If it does it drops it.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param table_name: Table Name
    '''
    for table_info in crsr.tables(tableType='TABLE'):
        if table_name == table_info.table_name:
            SQLstring = "DROP TABLE " + table_name
            crsr.execute(SQLstring)
            cnxn.commit()
            break

def GetConceptID(cnxn, crsr, category, parent_concept_id, code, label = None, value_type_concept_id = 1):
    '''
    Gets Concept id
    If it doesn't exist then adds it.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param category: String
    :param code: string
    :param label: string
    :param value_type_concept_id: string
    :return: integer
    '''

    SQLstring = "SELECT "
    SQLstring += "  concept_id "
    SQLstring += "FROM "
    SQLstring += "  ha_concepts "
    SQLstring += "WHERE "
    SQLstring += "  category = '" + category + "' "
    SQLstring += "  AND code = '" + code + "'"
    SQLstring += ";"

    crsr.execute(SQLstring)

    row = crsr.fetchone()

    #If concept doesn't exist then insert it
    if not row:

        SQLinsert = "INSERT INTO ha_concepts "
        SQLinsert += "  (category, parent_concept_id, code, label, value_type_concept_id) "
        SQLinsert += "VALUES "
        SQLinsert += "  ('" + category + "', " + str(parent_concept_id) + ", '" + code + "', " + return_null_string(label) + ", " + str(value_type_concept_id) + ")"
        SQLinsert += ";"

        crsr.execute(SQLinsert)
        cnxn.commit()

        #Re execute SQL select
        crsr.execute(SQLstring)

        row = crsr.fetchone()

    if row:
        return row.concept_id
    else:
        return 0

def GetStaffID(cnxn, crsr, staff_type_concept_id, staff_code, surname):
    '''
    Gets Staff id
    If person doesn't exist then add them.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param staff_type_concept_id: Integer
    :param staff_code: String
    :param surname: String
    :return: Integer
    '''

    SQLstring = "SELECT "
    SQLstring += "  staff_id "
    SQLstring += "FROM "
    SQLstring += "  ha_staff "
    SQLstring += "WHERE "
    SQLstring += "  staff_type_concept_id = " + str(staff_type_concept_id) + " "
    SQLstring += "  AND staff_code = '" + staff_code + "'"
    SQLstring += ";"

    crsr.execute(SQLstring)

    row = crsr.fetchone()

    #If concept doesn't exist then insert it
    if not row:

        SQLinsert = "INSERT INTO ha_staff "
        SQLinsert += "  (staff_type_concept_id, staff_code, surname) "
        SQLinsert += "VALUES "
        SQLinsert += "  (" + str(staff_type_concept_id) + ", '" + staff_code + "', " + return_null_string(surname) + ")"
        SQLinsert += ";"

        crsr.execute(SQLinsert)
        cnxn.commit()

        #Re execute SQL select
        crsr.execute(SQLstring)

        row = crsr.fetchone()

    if row:
        return row.staff_id
    else:
        return 0

def GetPatientID(cnxn, crsr, alt_health_identifier, health_identifier, first_name = None, surname = None, sex = None, birth_datetime = None, death_datetime = None):
    '''
    Gets Patient id
    If person doesn't exist then add them.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param health_identifier: Integer
    :param alt_health_identifier: String
    :param first_name: String
    :param surname: String
    :param sex: String
    :param birth_datetime: DateTime
    :param death_datetime: DateTime
    :return: Integer
    '''

    if not pandas.isnull(health_identifier):
        SQLstring = "SELECT "
        SQLstring += "  patient_id "
        SQLstring += "FROM "
        SQLstring += "  ha_patients "
        SQLstring += "WHERE "
        SQLstring += "   health_identifier = '" +  health_identifier + "' "
        SQLstring += ";"

        crsr.execute(SQLstring)

        row = crsr.fetchone()

        #If not existing as main identifier may exist as alt identifier if one given
        if not row and not pandas.isnull(alt_health_identifier):
            SQLstring = "SELECT "
            SQLstring += "  patient_id "
            SQLstring += "FROM "
            SQLstring += "  ha_patients "
            SQLstring += "WHERE "
            SQLstring += "   alt_health_identifier = '" +  alt_health_identifier + "' "
            SQLstring += ";"

            crsr.execute(SQLstring)

            row = crsr.fetchone()

    else:
        SQLstring = "SELECT "
        SQLstring += "  patient_id "
        SQLstring += "FROM "
        SQLstring += "  ha_patients "
        SQLstring += "WHERE "
        SQLstring += "   alt_health_identifier = '" +  alt_health_identifier + "' "
        SQLstring += ";"

        crsr.execute(SQLstring)

        row = crsr.fetchone()

    #If patient doesn't exist then insert it
    if not row:

        include_identifiers = False

        #ToDo could add birth & death datetime but chnaged to the 1st of the month.
        #ToDo shouldn't the health_identifier be ignored as well i.e. NHS Number

        if include_identifiers:

            SQLinsert = "INSERT INTO ha_patients "
            SQLinsert += "  ( health_identifier,  alt_health_identifier, surname, first_name, sex, birth_datetime, death_datetime) "
            SQLinsert += "VALUES "
            SQLinsert += "("
            SQLinsert += return_null_string(health_identifier) + ", " +  return_null_string(alt_health_identifier) + ", " + return_null_string(surname)
            SQLinsert += ", " + return_null_string(first_name) + ", " +  return_null_string(sex) + ", " + return_null_date(birth_datetime) + ", " + return_null_date(death_datetime)
            SQLinsert += ")"
            SQLinsert += ";"

        else:

            SQLinsert = "INSERT INTO ha_patients "
            SQLinsert += "  ( health_identifier,  alt_health_identifier, sex) "
            SQLinsert += "VALUES "
            SQLinsert += "("
            SQLinsert += return_null_string(health_identifier) + ", " +  return_null_string(alt_health_identifier) + ", " + return_null_string(sex)
            SQLinsert += ")"
            SQLinsert += ";"

        crsr.execute(SQLinsert)
        cnxn.commit()

        #Re execute SQL select
        crsr.execute(SQLstring)

        row = crsr.fetchone()

    if row:
        return row.patient_id
    else:
        return 0

def GetEventID(cnxn, crsr, event_type_concept_id, patient_id, start_date, staff_id = None, parent_event_id = None):
    '''
    Gets Concept id
    If it doesn't exist then adds it.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param event_type_concept_id: Integer
    :param patient_id: Integer
    :param start_date: Date
    :param staff_id: Integer (Optional)
    :param parent_event_id: Integer (Optional)
    :return: integer
    '''

    SQLinsert = "INSERT INTO ha_events "
    SQLinsert += "  (event_type_concept_id, patient_id, start_date, staff_id, parent_event_id) "
    SQLinsert += "VALUES "
    SQLinsert += "("
    SQLinsert += return_null_number(event_type_concept_id) + ", " + return_null_number(patient_id) + ", " + return_null_datetime(start_date)
    SQLinsert += ", " + return_null_number(staff_id) + ", " + return_null_number(parent_event_id)
    SQLinsert += ")"
    SQLinsert += ";"

    crsr.execute(SQLinsert)
    cnxn.commit()

    #execute SQL select
    SQLstring = "SELECT @@IDENTITY AS event_id"
    crsr.execute(SQLstring)

    row = crsr.fetchone()

    if row:
        return row.event_id
    else:
        return 0

def GetEventAttributeID(cnxn, crsr, event_id, category, code):
    '''
    Gets Event Attribute id
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :return: integer/None
    '''

    SQLstring = r"SELECT "
    SQLstring += r"  event_attribute_id "
    SQLstring += r"FROM ha_event_attributes AS EA"
    SQLstring += r"  LEFT OUTER JOIN ha_concepts AS CO "
    SQLstring += r"    ON CO.concept_id = EA.event_attribute_type_concept_id "
    SQLstring += r"WHERE "
    SQLstring += r"  EA.event_id = " + return_null_number(event_id) + " "
    SQLstring += r"  AND CO.category = " + return_null_string(category) + " "
    SQLstring += r"  AND CO.code = " + return_null_string(code) + " "
    SQLstring += r";"

    crsr.execute(SQLstring)

    return crsr.fetchone()


def CountPatientEventID(cnxn, crsr, patient_id, category, code, value_code = None):
    '''
    Gets Event Attribute id
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :return: integer/None
    '''

    SQLstring  = r"SELECT COUNT(event_id) AS events FROM ( "
    SQLstring += r"SELECT DISTINCT "
    SQLstring += r"  EV.event_id  "
    SQLstring += r"FROM "
    SQLstring += r"  ( "
    SQLstring += r"    ( "
    SQLstring += r"       ha_events AS EV INNER JOIN ha_concepts AS CO_EV ON EV.event_type_concept_id = CO_EV.concept_id "
    SQLstring += r"    ) "
    SQLstring += r"    INNER JOIN ha_event_attributes AS EA ON EV.event_id = EA.event_id "
    SQLstring += r"  ) "
    SQLstring += r"  INNER JOIN ha_concepts AS CO_EA ON EA.value_concept_id = CO_EA.concept_id "
    SQLstring += r"WHERE "
    SQLstring += r"  EV.patient_id = " + return_null_number(patient_id) + " "
    SQLstring += r"  AND CO_EV.category = " + return_null_string(category) + " "
    SQLstring += r"  AND CO_EV.code = " + return_null_string(code) + " "
    if value_code != None:
        SQLstring += r"  AND CO_EA.code LIKE '" + value_code + "%' "
    SQLstring += r")"
    SQLstring += r";"

    crsr.execute(SQLstring)

    return crsr.fetchone().events

def GetValueTypeConceptId(cnxn, crsr, value_type):

    if value_type == "TX":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "TEXT")
    elif value_type == "IN":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "INTEGER")
    elif value_type == "FL":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "FLOAT")
    elif value_type == "ID":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")
    elif value_type == "DA":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "DATE")
    elif value_type == "DT":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "DATETIME")
    elif value_type == "TM":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "TIME")
    elif value_type == "TD": # Time of day
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "TIME")
    elif value_type == "TF":
        value_type_concept_id = GetConceptID(cnxn, crsr, "/Concept/ValueType", None, "BOOLEAN")

    return value_type_concept_id

def AddPatientAttribute(cnxn, crsr, patient_id, attribute_code, attribute_label, value_type, value, label = None, check_unique = False):
    '''
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param patient_id: Integer
    :param attribute_code: String
    :param attribute_label: String
    :param value_type: String
    :param value: Variant
    :param label: String Optional

    Examples:
    AddPatientAttribute(cnxn, crsr, patient_id, "AC", "Age Category", "ID", "6", "Adult")
    AddPatientAttribute(cnxn, crsr, patient_id, "AG", "Age In Years", "IN", 62)


    '''

    value_type_concept_id = GetValueTypeConceptId(cnxn, crsr, value_type)

    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", None, "PatientAttribute")
    patient_attribute_type_concept_id = GetConceptID(cnxn, crsr, "/PatientAttribute", parent_concept_id, attribute_code, attribute_label, value_type_concept_id)

    insert_record = True

    if check_unique:
        SQLstring = "SELECT patient_attribute_id FROM ha_patient_attributes "
        SQLstring += "WHERE patient_id = " + return_null_number(patient_id) + " "
        SQLstring += "  AND patient_attribute_type_concept_id = " + return_null_number(patient_attribute_type_concept_id) + " "
        SQLstring += ";"

        crsr.execute(SQLstring)
        row = crsr.fetchone()
        if not pandas.isnull(row):
           insert_record = False

    if insert_record:

        SQLinsert = "INSERT INTO ha_patient_attributes "
        SQLinsert += "("
        SQLinsert += "patient_attribute_type_concept_id, patient_id"

        if value_type == "TX":
            SQLinsert += ", value_text"
        elif value_type in ("IN", "FL", "NU", "TF"):
            SQLinsert += ", value_numeric"
        elif value_type == "ID":
            SQLinsert += ", value_concept_id"
        elif value_type in ("DA", "DT", "TD", "TM"):
            SQLinsert += ", value_datetime"

        SQLinsert += ") "
        SQLinsert += "VALUES "
        SQLinsert += " ("
        SQLinsert += return_null_number(patient_attribute_type_concept_id) + ", " + return_null_number(patient_id)

        if value_type == "TX":
            SQLinsert += ", " + return_null_string(value)
        elif value_type in ("IN", "FL", "NU", "TF"):
            SQLinsert += ", " + return_null_number(value)
        elif value_type == "ID":
            if isinstance(value, int):
                if value <= 999:
                    code = ("000" + str(value))[-3:]
                else:
                    code = str(value)
            else:
                if value.isnumeric() and int(value) <= 999:
                    code = ("000" + value)[-3:]
                else:
                    code = value

            parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", None, "PatientAttribute")
            parent_concept_id = GetConceptID(cnxn, crsr, "/PatientAttribute",
                                             parent_concept_id, attribute_code, attribute_label, value_type_concept_id)

            value_concept_id = GetConceptID(cnxn, crsr, "/PatientAttribute/" + attribute_code, parent_concept_id, code, label, value_type_concept_id)
            SQLinsert += ", " + return_null_number(value_concept_id)

        elif value_type == "DA":
            SQLinsert += ", " + return_null_date(value)
        elif value_type in ("DT", "TD", "TM"):
            SQLinsert += ", " + return_null_datetime(value)

        SQLinsert += " )"
        SQLinsert += ";"

        # print(SQLinsert)

        crsr.execute(SQLinsert)
        cnxn.commit()


def AddEventAttribute(cnxn, crsr, event_id, event_type_category, attribute_code, attribute_label, value_type, value, label=None):
    '''
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param event_id: Integer
    :param attribute_code: String
    :param attribute_label: String
    :param value_type: String
    :param value: Variant
    :param label: String Optional

    AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "REF", "Referral", "ID", "1", "Sudden Death < 12 months")

    '''

    if gbl_add_profiling:
        start_time = time.time()

    value_type_concept_id = GetValueTypeConceptId(cnxn, crsr, value_type)

    # What is the parent concept?
    parent_category = event_type_category.rsplit("/",1)[0]
    parent_code = event_type_category.rsplit("/",1)[1]

    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/" + parent_category, None, parent_code)
    event_attribute_type_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/" + event_type_category, parent_concept_id,
                                             attribute_code, attribute_label,value_type_concept_id)

    SQLinsert = "INSERT INTO ha_event_attributes "
    SQLinsert += "("
    SQLinsert += "event_attribute_type_concept_id, event_id"

    if value_type == "TX":
        SQLinsert += ", value_text"
    elif value_type in ("IN", "FL", "NU", "TF"):
        # NB value_text used for units - if given
        SQLinsert += ", value_numeric, value_text"
    elif value_type == "ID":
        SQLinsert += ", value_concept_id"
    elif value_type in ("DA", "DT", "TD", "TM"):
        SQLinsert += ", value_datetime"

    SQLinsert += ") "
    SQLinsert += "VALUES "
    SQLinsert += " ("
    SQLinsert += return_null_number(event_attribute_type_concept_id) + ", " + return_null_number(event_id)

    if value_type == "TX":
        SQLinsert += ", " + return_null_string(value)
    elif value_type in ("IN", "FL", "NU", "TF"):
        SQLinsert += ", " + return_null_number(value) + ", " + return_null_string(label)
    elif value_type == "ID":

        if isinstance(value, int):
            if value <= 999:
                code = ("000" + str(value))[-3:]
            else:
                code = str(value)
        else:
            if value.isnumeric() and int(value) <= 999:
                code = ("000" + value)[-3:]
            else:
                code = value

        if event_type_category.find("/tbl") > 0:
            # cut out reference to table name for lookup
            parent_category = "/EventAttribute/" + event_type_category[:event_type_category.find("/tbl")] + "/LookUp"
        else:
            parent_category = "/EventAttribute/" + event_type_category + "/LookUp"

        category = parent_category + "/" + attribute_code

        parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None,"PostMortem")
        parent_concept_id = GetConceptID(cnxn, crsr, parent_category,
                                         parent_concept_id, attribute_code, attribute_label, value_type_concept_id)

        value_concept_id = GetConceptID(cnxn, crsr, category, parent_concept_id, code, label, value_type_concept_id)

        SQLinsert += ", " + return_null_number(value_concept_id)

    elif value_type == "DA":
        SQLinsert += ", " + return_null_date(value)
    elif value_type in ("DT", "TD", "TM"):
        SQLinsert += ", " + return_null_datetime(value)

    SQLinsert += " )"
    SQLinsert += ";"

    crsr.execute(SQLinsert)
    cnxn.commit()

    if gbl_add_profiling:
        end_time = time.time()
        # print(event_id, value_type, f' - Add Event Attribute: {end_time - start_time:.4f} secs', "\Event Attribute Type\\" + event_type_category, attribute_code, attribute_label)


def BuildEventsSQL(cnxn, crsr, ReportTableID, SystemTableID, SystemTableName, CaseID = None):

    '''
    Returns SQL string
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    :param ReportTableID: Integer
    :param SystemTableID: Integer
    :param SystemTableName: String
    :param CaseID: Integer (Optional)
    :return: String

    '''

    SQLQuery = "SELECT tblCases.CaseID "

    if SystemTableName != "tblCases":

        #Add Primary key to determine NULL record - if not Cases table
        #Get the name of the First field in the Table

        SQLstring = "select SystemFieldName from tblSystemFields where SystemTableID = " + str(SystemTableID) + " ORDER BY SystemFieldID;"
        crsr.execute(SQLstring)
        row = crsr.fetchone()

        PrimaryKeyName = row.SystemFieldName

        SQLQuery += ", tblCases.PMNumber, " + SystemTableName + "." + PrimaryKeyName + " "

    else:
        PrimaryKeyName = ""

    #Get Fields for CurrentTable
    SQLstring = "SELECT ReportTableCode, SystemFieldName, tblReportFields.IncludeInReport "
    SQLstring += "FROM (tblSystemTables INNER JOIN tblReportTables ON tblSystemTables.SystemTableID = tblReportTables.SystemTableID) INNER JOIN (tblSystemFields INNER JOIN tblReportFields ON tblSystemFields.SystemFieldID = tblReportFields.SystemFieldID) ON tblSystemTables.SystemTableID = tblSystemFields.SystemTableID "
    SQLstring += "WHERE tblReportTables.ReportTableID = " + str(ReportTableID) + "  And tblReportFields.IncludeInEventAttributes = True "
    SQLstring += "ORDER BY ReportFieldOrder;"

    crsr.execute(SQLstring)
    rows = crsr.fetchall()

    for row in rows:
        # For CSV files want CaseID for joining tables but can't have it twice
        if row.SystemFieldName != "CaseID":
            SQLQuery += ", " + SystemTableName + "." + row.SystemFieldName + " "

    if SystemTableName in ("tblCardiovascularSystems", "tblRespiratorySystems", "tblGastrointestinalSystems", "tblReticuloendothelialSystems", "tblEndocrineSystems", "tblUrogenitalSystems", "tblCentralNervousSystems"):

        SQLQuery += "FROM (tblCases INNER JOIN (tblAutopsies INNER JOIN tblInternalExams "
        SQLQuery += "ON tblAutopsies.AutopsyID = tblInternalExams.AutopsyID) "
        SQLQuery += "ON tblCases.CaseID = tblAutopsies.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblInternalExams.InternalExamID = " + SystemTableName + ".InternalExamID "

    elif SystemTableName in ("tblExternalExams", "tblInternalExams"):

        SQLQuery += "FROM (tblCases INNER JOIN tblAutopsies "
        SQLQuery += "ON tblCases.CaseID = tblAutopsies.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblAutopsies.AutopsyID = " + SystemTableName + ".AutopsyID "

    elif SystemTableName in ("tblImpBacteriology", "tblImpVirology", "tblImpCSFMicroscopy", "tblImpPhageType"):

        SQLQuery += "FROM (tblCases INNER JOIN tblMicrobiology "
        SQLQuery += "ON tblCases.CaseID = tblMicrobiology.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblMicrobiology.MicrobiologyID = " + SystemTableName + ".MicrobiologyID "

    elif SystemTableName in ("tblImpGuthries", "tblImpVitreousHumour"):

        SQLQuery += "FROM (tblCases INNER JOIN tblMetabolic "
        SQLQuery += "ON tblCases.CaseID = tblMetabolic.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblMetabolic.MetabolicID = " + SystemTableName + ".MetabolicID "


    elif SystemTableName in ("tblToxicologyResults"):

        SQLQuery += "FROM (tblCases INNER JOIN tblToxicology "
        SQLQuery += "ON tblCases.CaseID = tblToxicology.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblToxicology.ToxicologyID = " + SystemTableName + ".ToxicologyID "

    elif SystemTableName in ("tblXRays"):

        SQLQuery += "FROM (tblCases INNER JOIN tblRadiology "
        SQLQuery += "ON tblCases.CaseID = tblRadiology.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblRadiology.RadiologyID = " + SystemTableName + ".RadiologyID "

    elif SystemTableName in ("tblAntenatalDetails", "tblCauseOfLoss", "tblDeliveryDetails", "tblPlacenta", "tblRiskFactors"):

        SQLQuery += "FROM (tblCases INNER JOIN tblFoetal "
        SQLQuery += "ON tblCases.CaseID = tblFoetal.CaseID) "
        SQLQuery += "INNER JOIN " + SystemTableName + " ON tblFoetal.FoetalID = " + SystemTableName + ".FoetalID "

    elif SystemTableName in ("tblCases"):

        SQLQuery += "FROM tblCases "

    else:

        SQLQuery += "FROM tblCases LEFT JOIN " + SystemTableName + " ON tblCases.CaseID = " + SystemTableName + ".CaseID "

    SQLQuery += "WHERE tblCases.CaseID = " + str(CaseID) + "  "
    SQLQuery += ";"

    return SQLQuery


def create_has_tables(cnxn, crsr):

    '''

    :param cnxn: Connection to database
    :param crsr: Cursor for all SQL operations

    Comments:   Creates HAS tables in ACCESS DB
                Currently at HAS 1.08
                This is not a complete HAS implementation; just the tables required for this project.
    '''

    drop_table(cnxn, crsr, "ha_event_attributes")
    drop_table(cnxn, crsr, "ha_events")
    drop_table(cnxn, crsr, "ha_patient_attributes")
    drop_table(cnxn, crsr, "ha_patients")
    drop_table(cnxn, crsr, "ha_staff")
    drop_table(cnxn, crsr, "ha_concepts")

    SQLstring = "CREATE TABLE ha_concepts ( "
    SQLstring += "concept_id             AUTOINCREMENT PRIMARY KEY, "
    # These should be NOT NULL but can't add self referential entry as first concept, added in next stage.
    SQLstring += "parent_concept_id      INTEGER NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "value_type_concept_id  INTEGER NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "concept_uri            VARCHAR(255) NULL, "
    SQLstring += "code                   VARCHAR(255) NULL, "
    SQLstring += "term                   VARCHAR(255) NULL, "
    SQLstring += "label                  VARCHAR(255) NULL, "
    SQLstring += "concept_note           VARCHAR(255) NULL, "   # MS Access doesn't support a column called 'note'
    SQLstring += "category               VARCHAR(255) NOT NULL, "
    SQLstring += "created                DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "CREATE TABLE ha_staff ( "
    SQLstring += "staff_id              AUTOINCREMENT PRIMARY KEY, "
    SQLstring += "staff_code            VARCHAR(100) NULL, "
    SQLstring += "full_name             VARCHAR(100) NULL, "
    SQLstring += "first_name            VARCHAR(50) NULL, "
    SQLstring += "middle_names          VARCHAR(50) NULL, "
    SQLstring += "surname               VARCHAR(50) NULL, "
    SQLstring += "staff_type_concept_id INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "start_date            DATETIME NULL, "
    SQLstring += "end_date              DATETIME NULL, "
    SQLstring += "created               DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "CREATE TABLE ha_patients ( "
    SQLstring += "patient_id            AUTOINCREMENT PRIMARY KEY, "
    SQLstring += "first_name            VARCHAR(255) NULL, "
    SQLstring += "middle_names          VARCHAR(255) NULL, "
    SQLstring += "surname               VARCHAR(255) NULL, "
    SQLstring += "sex                   VARCHAR(1) NULL, "
    SQLstring += "birth_datetime        DATETIME NULL, "
    SQLstring += "death_datetime        DATETIME NULL, "
    SQLstring += "deceased_flag         INTEGER NULL, "
    SQLstring += "postcode              VARCHAR(25) NULL, "
    SQLstring += "patient_zone          VARCHAR(255) NULL, "   # MS Access doesn't support a column called 'zone'
    SQLstring += "health_identifier     VARCHAR(255) NULL, "
    SQLstring += "alt_health_identifier VARCHAR(255) NULL, "
    SQLstring += "project_code          VARCHAR(100) NULL DEFAULT GOSH_DRE, "   # MS Access doesn't support a DEFAULT text with a space.
    SQLstring += "created               DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "CREATE TABLE ha_patient_attributes ( "
    SQLstring += "patient_attribute_id              AUTOINCREMENT PRIMARY KEY, "
    SQLstring += "parent_patient_attribute_id       INTEGER NULL REFERENCES ha_patient_attributes(patient_attribute_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "patient_id                        INTEGER NOT NULL REFERENCES ha_patients(patient_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "patient_attribute_type_concept_id INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "sequence_number                   INTEGER NULL, "
    SQLstring += "value_text                        VARCHAR(255) NULL, "
    SQLstring += "value_numeric                     DECIMAL(18,4) NULL, "
    SQLstring += "value_datetime                    DATETIME NULL, "
    SQLstring += "value_boolean                     INTEGER NULL, "
    SQLstring += "value_concept_id                  INTEGER NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "created                           DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "CREATE TABLE ha_events ( "
    SQLstring += "event_id              AUTOINCREMENT PRIMARY KEY, "
    SQLstring += "parent_event_id       INTEGER NULL REFERENCES ha_events(event_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "patient_id            INTEGER NULL REFERENCES ha_patients(patient_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "staff_id              INTEGER NULL REFERENCES ha_staff(staff_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "event_type_concept_id INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "start_date            DATETIME NULL, "
    SQLstring += "end_date              DATETIME NULL, "
    SQLstring += "event_note            VARCHAR(255) NULL, "  # MS Access doesn't support a column called 'note'
    SQLstring += "created               DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "CREATE TABLE ha_event_attributes ( "
    SQLstring += "event_attribute_id              AUTOINCREMENT PRIMARY KEY, "
    SQLstring += "parent_event_attribute_id       INTEGER NULL REFERENCES ha_event_attributes(event_attribute_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "event_id                        INTEGER NOT NULL REFERENCES ha_events(event_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "event_attribute_type_concept_id INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "sequence_number                 INTEGER NULL, "
    SQLstring += "value_text                      VARCHAR(255) NULL, "
    SQLstring += "value_numeric                   DECIMAL(18,4) NULL, "
    SQLstring += "value_datetime                  DATETIME NULL, "
    SQLstring += "value_concept_id                INTEGER NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
    SQLstring += "created                         DATETIME DEFAULT NOW() NOT NULL "
    SQLstring += ");"
    crsr.execute(SQLstring)
    cnxn.commit()

    # Create basic concepts required for all further processes
    create_concept_id(cnxn, crsr)

    create_core_concepts(cnxn, crsr)


def create_concept_id(cnxn, crsr):
    '''
    Adds initial concept which is self refferntial and then adds NOT NULL constraint.
    :param cnxn: ODBC Connection
    :param crsr: ODBC Cursor
    '''

    SQLstring = "INSERT INTO ha_concepts "
    SQLstring += "  (category, parent_concept_id, code, label, value_type_concept_id) "
    SQLstring += "VALUES "
    SQLstring += "  ('/', NULL, 'Concept', 'Concept', NULL)"
    SQLstring += ";"

    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "UPDATE ha_concepts "
    SQLstring += "  SET parent_concept_id = 1, value_type_concept_id = 1 "
    SQLstring += "WHERE concept_id = 1 "
    SQLstring += ";"

    crsr.execute(SQLstring)
    cnxn.commit()

    # Add NOT NULL constraint to columns

    SQLstring = "ALTER TABLE ha_concepts "
    SQLstring += "  ALTER COLUMN parent_concept_id INTEGER NOT NULL"
    SQLstring += ";"

    crsr.execute(SQLstring)
    cnxn.commit()

    SQLstring = "ALTER TABLE ha_concepts "
    SQLstring += "  ALTER COLUMN value_type_concept_id INTEGER NOT NULL"
    SQLstring += ";"

    crsr.execute(SQLstring)
    cnxn.commit()

def create_core_concepts(cnxn, crsr):
    '''

    :param cnxn: Connection to database
    :param crsr: Cursor for all SQL operations

    Comments:   Creates core concepts required for all other concepts
    '''

    value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")
    parent_concept_id = value_type_concept_id

    # Concept value type: ValueType
    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", parent_concept_id, "ValueType", "Value Type", value_type_concept_id)

    # Concept value type: Integer
    value_type_integer = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "INTEGER", "Integer", value_type_concept_id)

    # Concept value type: Float
    value_type_float = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "FLOAT", "Float", value_type_concept_id)

    # Concept value type: Date
    value_type_date = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "DATE", "Date", value_type_concept_id)

    # Concept value type: Time
    value_type_time = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "TIME", "Time", value_type_concept_id)

    # Concept value type: DateTime
    value_type_datetime = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "DATETIME", "DateTime", value_type_concept_id)

    # Concept value type: Text
    value_type_text = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "TEXT", "Text", value_type_concept_id)

    # Concept value type: Boolean
    value_type_boolean = GetConceptID(cnxn, crsr, "/Concept/ValueType", parent_concept_id, "BOOLEAN", "Boolean", value_type_concept_id)

    #------------------------------------------------
    #--Staff Types and values
    #------------------------------------------------

    parent_concept_id = value_type_concept_id

    # Concept value type: Staff
    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", parent_concept_id, "Staff", "Staff", value_type_concept_id)

    # Concept value type: StaffType
    parent_concept_id = GetConceptID(cnxn, crsr, "/Staff", parent_concept_id, "StaffType", "StaffType", value_type_concept_id)

    # Concept value type: Consulatnt
    parent_concept_id = GetConceptID(cnxn, crsr, "/Staff/StaffType", parent_concept_id, "Consultant", "Consultant", value_type_concept_id)


    #------------------------------------------------
    #--Event Types and values
    #------------------------------------------------

    parent_concept_id = value_type_concept_id

    # Concept value type: Event
    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", parent_concept_id, "Event", "Event", value_type_concept_id)

    # Concept value type: Observation
    parent_concept_id = GetConceptID(cnxn, crsr, "/Event", parent_concept_id, "Observation", "Observation", value_type_concept_id)

    # Concept value type: Post Mortem
    parent_concept_id = GetConceptID(cnxn, crsr, "/Event/Observation", parent_concept_id, "PostMortem", "Post Mortem", value_type_concept_id)

    #------------------------------------------------
    #--Event Attribute Types and values
    #------------------------------------------------

    parent_concept_id = value_type_concept_id

    # Concept value type: Event
    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", parent_concept_id, "EventAttribute", "Event Attribute", value_type_concept_id)

    # Concept value type: Observation
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute", parent_concept_id, "Observation", "Observation", value_type_concept_id)

    # Concept value type: Post Mortem
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", parent_concept_id, "PostMortem", "Post Mortem", value_type_concept_id)

    # Concept value type: Look up
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "Lookup", "Look up", value_type_concept_id)


    # ------------------------------------------------
    # --Patient Attribute Types and values
    # ------------------------------------------------

    parent_concept_id = value_type_concept_id

    # Concept value type: Event
    parent_concept_id = GetConceptID(cnxn, crsr, "/Concept", parent_concept_id, "PatientAttribute", "Patient Attribute",
                                     value_type_concept_id)

    # Concept value type: Look up
    parent_concept_id = GetConceptID(cnxn, crsr, "/PatientAttribute", parent_concept_id, "Lookup",
                                     "Look up", value_type_concept_id)


def runTests(cnxn, crsr):

    # Get staff type ID for consultants
    staff_type_concept_id = GetConceptID(cnxn, crsr, "/Staff/StaffType", None, "Consultant")

    # Get staff ID for a consultant
    staff_id = GetStaffID(cnxn, crsr, staff_type_concept_id, "CN99999", "Booth")
    print(staff_id)
    # Check for no duplication
    staff_id = GetStaffID(cnxn, crsr, staff_type_concept_id, "CN99999", "Booth")
    print(staff_id)

    # Get patient ID
    patient_id = GetPatientID(cnxn, crsr, "101101", "99999999999", "John", "Booth", "M", datetime.date(1956, 12, 11))
    print(patient_id)
    patient_id = GetPatientID(cnxn, crsr, "101101", "99999999999", "John", "Booth", "M", datetime.date(1956, 12, 11))
    print(patient_id)

    # Get patient ID
    patient_id = GetPatientID(cnxn, crsr, None, "99999999998", "Christine", "Booth", "F", datetime.date(1955, 3, 5))
    print(patient_id)
    patient_id = GetPatientID(cnxn, crsr, None, "99999999998", "Christine", "Booth", "F", datetime.date(1955, 3, 5))
    print(patient_id)

    # Get patient ID
    patient_id = GetPatientID(cnxn, crsr, "59678", None, "Nick", "Booth", "M", datetime.date(1990, 12, 22))
    print(patient_id)
    patient_id = GetPatientID(cnxn, crsr, "59678", None, "Nick", "Booth", "M", datetime.date(1990, 12, 22))
    print(patient_id)

    # Get patient ID
    patient_id = GetPatientID(cnxn, crsr, "123456", None, None, "Booth")
    print(patient_id)
    patient_id = GetPatientID(cnxn, crsr, "123456", None, None, "Booth")
    print(patient_id)

    # Get event type ID for post mortem
    event_type_concept_id = GetConceptID(cnxn, crsr, "/Event/Observation", None, "PostMortem")

    event_id = GetEventID(cnxn, crsr, event_type_concept_id, patient_id, datetime.datetime.now())
    print(event_id)

    AddPatientAttribute(cnxn, crsr, patient_id, "AC", "Age Category", "ID", "6", "Adult")
    AddPatientAttribute(cnxn, crsr, patient_id, "AG", "Age In Years", "IN", 62)


    # Concept value type: Post Mortem
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")

    # Concept value type: tblCases
    value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "tblCases", "tblCases", value_type_concept_id)

    AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "CASEID", "Case ID", "IN", 1001)
    AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "PMNumber", "PM Number", "TX", "17P564")
    AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "REF", "Referral", "ID", "1", "Sudden Death < 12 months")
    AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem", "NA", "Number of Attributes", "ID", "1", "Less than 10")

    SQLstring = BuildEventsSQL(cnxn, crsr, 5, 77, "tblCardiovascularSystems", 1)
    crsr.execute(SQLstring)
    row = crsr.fetchone()

    print(row.PMNumber)  # Should be 00P001

    print(GetAgeCategory(None,234,""))
    print(GetAgeCategoryID(None,234,""))
    print(GetPMYear("95P000"))
    print(GetPMYear("21P000"))
    print(GetPMYear("18P000"))
    print(GetPMYear("9P5000"))

def CreateHASCSVFiles(cnxn: object, crsr: object, destination_folder):
    '''

    :param cnxn:
    :param crsr:

    '''

    SQLstring = "SELECT * FROM ha_concepts ORDER BY concept_id;"
    file_name = "ha_concepts_import"
    table_name = "ha_concepts"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)

    SQLstring = "SELECT * FROM ha_staff ORDER BY staff_id"
    file_name = "ha_staff_import"
    table_name = "ha_staff"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)

    SQLstring = "SELECT * FROM ha_patients ORDER BY patient_id"
    file_name = "ha_patients_import"
    table_name = "ha_patients"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)

    SQLstring = "SELECT * FROM ha_patient_attributes ORDER BY patient_attribute_id;"
    file_name = "ha_patient_attributes_import"
    table_name = "ha_patient_attributes"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)

    SQLstring = "SELECT * FROM ha_events ORDER BY event_id"
    file_name = "ha_events_import"
    table_name = "ha_events"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)

    SQLstring = "SELECT * FROM ha_event_attributes ORDER BY event_attribute_id"
    file_name = "ha_event_attributes_import"
    table_name = "ha_event_attributes"

    CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder)


def CreateCSVFileFromSQL(cnxn, crsr, SQLstring, file_name, table_name, destination_folder):
    '''

    :param cnxn:
    :param crsr:
    :param SQLstring:
    :param file_name:
    :return:
    '''
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

    # Get column Names
    writer.writerow([x[0] for x in crsr.description])

    Rows = crsr.fetchall()

    row_counter = 0
    print("")
    print("Outputting to table: %s to file: %s" % (table_name,destination_folder + file_name + file_ext))
    for Row in Rows:

        row_counter += 1

        sys.stdout.write("\r \r {0}".format(str(row_counter)))
        sys.stdout.flush()

        column_pos = 0
        out_row = []

        for column in crsr.description:

            if pandas.isnull(Row[column_pos]):
                out_row.append('NULL')
            else:

                if column[1] == str:
                    out_row.append(Row[column_pos])
                else:
                    out_row.append(Row[column_pos])

            column_pos += 1

        writer.writerow(out_row)

    # crsr.close()

    file.close()
    print("")
    print("file closed")

    #Create XML file

    XMLLine =  '<?xml version="1.0" encoding="UTF-8"?>'
    XMLLine += '<DatasetDefinition xmlns="http://aridhia-mgrid.com/ddf/2" TableName="' + file_name + '" Action="create">'
    XMLLine += '<Title>' + file_name + '</Title>'
    XMLLine += '<Format Delimiter="," TextQualifier="&quot;" Encoding="UTF-8" HeaderCase="leave alone" Header="true" CopyToFile="true" NullQualifier="NULL"/>'
    XMLLine += '<Columns>'

    for column in crsr.description:

        # print(column[0], column[1], column[2], column[3], column[4], column[5], column[6])

        column_name = column[0]

        if column[1] == int:
            column_type = "integer"
        elif column[1] == decimal.Decimal:
            column_type = "double precision"
        elif column[1] == datetime.date:
            column_type = "date"
        elif column[1] == datetime.datetime:   # All access date/timestamp return datetime
            if column_name.upper().find(("_DATE")) >= 0:
                column_type = "date"
            else:
                column_type = "timestamp"
        elif column[1] == str:
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
        new_fname = file_name + "_" + file_time_stamp() + file_ext
        os.rename(destination_folder + file_name + file_ext, destination_folder + new_fname)

    file = open(destination_folder + file_name + file_ext, 'w', newline='', encoding='utf-8')

    print("")
    print("Outputting to file: %s" % (destination_folder + file_name + file_ext))

    file.write(XMLLine)

    file.close()
    print("")
    print("file closed")


def CreateAttributeNoOfAttributes(cnxn, crsr):
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

        AddEventAttribute(cnxn, crsr, EventAttributeRow.event_id, "Observation/PostMortem", "ATTRIBUTES", "Number of attributes", "IN", EventAttributeRow.Attributes)

    print("")
    print("Done!")

def CreateCOD2_SUMMAttributeFromCOD2Attribute(cnxn, crsr):
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
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")

    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "tblFinalDiagnoses", "tblFinalDiagnoses", value_type_concept_id)

    event_attribute_type_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/tblFinalDiagnoses", parent_concept_id, "COD2_SUMM", "COD2_SUMM", value_type_concept_id)

    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", None, "LookUp")
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp", parent_concept_id, "COD2_SUMM", "COD2_SUMM", value_type_concept_id)

    value_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "001", "Unknown", value_type_concept_id)
    value_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "002", "known", value_type_concept_id)
    value_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "003", "Other", value_type_concept_id)
    value_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem/LookUp/COD2_SUMM", parent_concept_id, "994", "N/A", value_type_concept_id)

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
        AddEventAttribute(cnxn, crsr, EventAttributeRow.event_id, "Observation/PostMortem/tblFinalDiagnoses", "COD2_SUMM", "COD2_SUMM", "ID", code, text)

    print("")
    print("Done!")


def CreateLabEvents(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''

    start_row = 0
    MaxRows = 999999

    ST_Event_type_id = GetConceptID(cnxn, crsr, "\\Event Type", "ST", "Specimen Taken")
    LE_Event_type_id = GetConceptID(cnxn, crsr, "\\Event Type", "LE", "Lab Episode")
    LT_Event_type_id = GetConceptID(cnxn, crsr, "\\Event Type", "LT", "Lab Test")
    LR_Event_type_id = GetConceptID(cnxn, crsr, "\\Event Type", "LR", "Lab Test Result")

    cnt_specimentypecode = ""
    cnt_sitecode = ""
    cnt_episodecommenttext = ""

    cnt_labno = ""
    cnt_testsetcode = ""

    ST_event_id = 0
    LE_event_id = 0
    LT_event_id = 0

    SQLstring = "SELECT "
    SQLstring += "  labno, "
    SQLstring += "  hospitalno, "
    SQLstring += "  wardcode, "
    SQLstring += "  specimentypecode, "
    SQLstring += "  specimentypename, "
    SQLstring += "  sitecode, "
    SQLstring += "  sitedescription, "
    SQLstring += "  episodecommentcode, "
    SQLstring += "  episodecommenttext, "
    SQLstring += "  collected, "
    SQLstring += "  testsetcode, "
    SQLstring += "  testsetname, "
    SQLstring += "  testcode, "
    SQLstring += "  testliteral, "
    SQLstring += "  resultcode, "
    SQLstring += "  resulttext "
    SQLstring += "FROM "
    SQLstring += "  histo_microbiology_virology_sample_test_results_02 "
    SQLstring += ";"

    crsr.execute(SQLstring)
    LabResultRows = crsr.fetchall()

    row = 0
    print("Processing Lab Events")
    first_time = time.time()
    last_time = time.time()

    for LabResultRow in LabResultRows:

        row += 1
        if row >= start_row:

            print(row, LabResultRow.labno, f'{time.time() - last_time:.3f} result secs', f'{time.time() - first_time:.1f} total secs')
            last_time = time.time()

            patient_id = GetPatientID(cnxn, crsr, LabResultRow.hospitalno, LabResultRow.labno, None, None, None, None)

            # Must have a valid date to create any events

            ValidRecord = True

            event_start_date = Nz(LabResultRow.collected, "NULL")

            if event_start_date == "NULL" or not isDate(LabResultRow.collected[:16]):
                ValidRecord = False

            if ValidRecord:

                event_start_date = datetime.datetime.strptime(LabResultRow.collected[:16], '%Y-%m-%d %H:%M')

                if cnt_specimentypecode != LabResultRow.specimentypecode or cnt_sitecode != Nz(LabResultRow.sitecode, "NULL") or cnt_episodecommenttext != Nz(LabResultRow.episodecommenttext, "NULL"):

                    cnt_specimentypecode = LabResultRow.specimentypecode
                    cnt_sitecode = Nz(LabResultRow.sitecode, "NULL")
                    cnt_episodecommenttext = Nz(LabResultRow.episodecommenttext, "NULL")

                    #Create take specimen event
                    ST_event_id = GetEventID(cnxn, crsr, ST_Event_type_id, patient_id, event_start_date)

                    #Create take specimen event attributes
                    AddEventAttribute(cnxn, crsr, ST_event_id, "Specimen Taken", "SPECIMEN_TYPE", "Specimen Type", "ID", LabResultRow.specimentypecode, LabResultRow.specimentypename)
                    if Nz(LabResultRow.sitecode, "NULL") != "NULL":
                        AddEventAttribute(cnxn, crsr, ST_event_id, "Specimen Taken", "SITE", "Site", "ID", LabResultRow.sitecode, LabResultRow.sitedescription)

                    if Nz(LabResultRow.episodecommenttext, "NULL") != "NULL":
                        AddEventAttribute(cnxn, crsr, ST_event_id, "Specimen Taken", "COMMENT", "Comment", "TX", LabResultRow.episodecommenttext)

                if cnt_labno != LabResultRow.labno:

                    cnt_labno = LabResultRow.labno

                    #Create Lab Episode event
                    LE_event_id = GetEventID(cnxn, crsr, LE_Event_type_id, patient_id, event_start_date, None, ST_event_id)

                    #Create lab test event attributes
                    AddEventAttribute(cnxn, crsr, LE_event_id, "Lab Episode", "LABNO", "Lab No", "TX", LabResultRow.labno)

                if cnt_testsetcode != LabResultRow.testsetcode:

                    cnt_testsetcode = LabResultRow.testsetcode

                    #Create Lab Test event
                    LT_event_id = GetEventID(cnxn, crsr, LT_Event_type_id, patient_id, event_start_date, None, LE_event_id)

                    #Create lab test event attributes
                    AddEventAttribute(cnxn, crsr, LT_event_id, "Lab Test", "TEST SET", "Test Set", "ID", LabResultRow.testsetcode, LabResultRow.testsetname)

                # if len(Nz(LabResultRow.testliteral, "").strip()) < 2 and len(Nz(LabResultRow.resultcode, "").strip()) == 0 and len(Nz(LabResultRow.resulttext, "").strip()) == 0:
                # If no result then not interested
                if len(Nz(LabResultRow.resultcode, "").strip()) == 0 and len(Nz(LabResultRow.resulttext, "").strip()) == 0:
                        ValidRecord = False

                if ValidRecord:

                    #Create lab test results event
                    LR_event_id = GetEventID(cnxn, crsr, LR_Event_type_id, patient_id, event_start_date, None, LT_event_id)

                    #Create lab test results event attributes
                    AddEventAttribute(cnxn, crsr, LR_event_id, "Lab Test Result", "TEST", "Test", "ID", LabResultRow.testcode, Nz(LabResultRow.testliteral, ""))
                    if len(Nz(LabResultRow.resultcode, "").strip()) != 0:
                        AddEventAttribute(cnxn, crsr, LR_event_id, "Lab Test Result", "RESULTCODE", "Result code", "TX", Nz(LabResultRow.resultcode, ""))

                    if len(Nz(LabResultRow.resulttext, "").strip()) != 0:
                        AddEventAttribute(cnxn, crsr, LR_event_id, "Lab Test Result", "RESULTTEXT", "Result text", "TX", Nz(LabResultRow.resulttext, ""))

        else:
            print(row, LabResultRow.labno, 'Skipping Row')

        if row >= MaxRows:
            break


def create_reporting_attributes(cnxn, crsr):
    '''
    :param cnxn:
    :param crsr:
    :return:

    '''

    # Makre sure that PostMortem/Reporting exists - ToDo move to create_core_concepts
    value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, "Reporting", "Reporting", value_type_concept_id)

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

            event_attribute_id = GetEventAttributeID(cnxn, crsr, EventRow.event_id, category, code)

            if event_attribute_id != None:
                AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "ExternalExam", "External Examination", "TF", 1)
            else:
                AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "ExternalExam", "External Examination", "TF", 0)

            # Defined as having a Internal exam if Heart weight is greater than 0
            category = r"/EventAttribute/Observation/PostMortem/tblInternalExams"
            code = r"HeartWeight"

            event_attribute_id = GetEventAttributeID(cnxn, crsr, EventRow.event_id, category, code)

            if event_attribute_id != None:
                AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "InternalExam", "Internal Examination", "TF", 1)
            else:
                AddEventAttribute(cnxn, crsr, EventRow.event_id, r"Observation/PostMortem/Reporting", "InternalExam", "Internal Examination", "TF", 0)

            # Get number of samples taken
            category = r"/Event/Observation"
            code = r"LAB_EPISODE"

            events = CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code)

            if events > 0:
                #Get number of samples
                AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "SAMPLETKN", "Samples Taken", "IN", events)

                # Defined as having Microbiology if any Test Sets B*
                category = r"/Event/Observation"
                code = r"LT"
                value_code = r"B"

                events = CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code, value_code)

                if events != None:
                    AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "MICRO", "Microbiology Tests", "TF", 1)
                else:
                    AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "MICRO", "Microbiology Tests", "TF", 0)

                # Defined as having Microbiology if any Test Sets B*
                category = r"/Event/Observation"
                code = r"LT"
                value_code = r"V"

                events = CountPatientEventID(cnxn, crsr, EventRow.patient_id, category, code, value_code)

                if events != None:
                    AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "VIROLOGY", "Virology Tests", "TF", 1)
                else:
                    AddEventAttribute(cnxn, crsr, EventRow.event_id, r"PostMortem/Reporting", "VIROLOGY", "Virology Tests", "TF", 0)

    print("")
    print("Done!")


def CreateEvents(cnxn, crsr, res_crsr, max_rows = 999999):

    global gbl_add_event_time

    #Setup
    SQLstring = "SELECT "
    SQLstring += "tblReportTables.ReportTableID, tblReportTables.SystemTableID, SystemTableName "
    SQLstring += "FROM tblReportTables "
    SQLstring += "  INNER JOIN tblSystemTables ON tblReportTables.SystemTableID = tblSystemTables.SystemTableID "
    SQLstring += "WHERE IncludeInReport "
    SQLstring += "ORDER BY ReportTableOrder;"

    crsr.execute(SQLstring)
    ReportTableRows = crsr.fetchall()

    #Check concepts are created for every table to be used
    parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation", None, "PostMortem")
    value_type_concept_id = GetConceptID(cnxn, crsr, "/", None, "Concept")
    for ReportTableRow in ReportTableRows:
        parent_concept_id = GetConceptID(cnxn, crsr, "/EventAttribute/Observation/PostMortem", parent_concept_id, ReportTableRow.SystemTableName, ReportTableRow.SystemTableName, value_type_concept_id)

    SQLstring = "SELECT tblReportTables.ReportTableID, tblReportTables.ReportTableCode, tblSystemFields.SystemFieldName, tblReportFields.IncludeInReport, tblSystemFields.SystemLKTableID, tblSystemFields.SystemFieldType, tblSystemTables_1.SystemTableName AS SystemLKTableName "
    SQLstring += "FROM ((tblSystemTables INNER JOIN tblReportTables ON tblSystemTables.SystemTableID = tblReportTables.SystemTableID) INNER JOIN (tblSystemFields INNER JOIN tblReportFields ON tblSystemFields.SystemFieldID = tblReportFields.SystemFieldID) ON tblSystemTables.SystemTableID = tblSystemFields.SystemTableID) LEFT JOIN tblSystemTables AS tblSystemTables_1 ON tblSystemFields.SystemLkTableID = tblSystemTables_1.SystemTableID "
    SQLstring += "WHERE tblReportFields.IncludeInEventAttributes = True "
    SQLstring += "ORDER BY tblReportTables.ReportTableID, ReportFieldOrder;"

    crsr.execute(SQLstring)
    ReportTableFieldRows = crsr.fetchall()

    # Get event type ID for post mortem
    event_type_concept_id = GetConceptID(cnxn, crsr, "/Event/Observation", None, "PostMortem")
    # Get staff type ID for consultants
    staff_type_concept_id = GetConceptID(cnxn, crsr, "/Staff/StaffType", None, "Consultant")

    #Before you can create an event you need the patient and the member of staff

    #Require an initial query that gives base details for event plus patient and staff details

    SQLstring = "SELECT tblCases.CaseID, "
    SQLstring += " tblCases.PMNumber, "
    SQLstring += " tblAutopsies.DateAutopsy, "
    SQLstring += " tblCases.HospitalNo, "
    SQLstring += " tblCases.Surname, "
    SQLstring += " tblCases.FirstName, "
    SQLstring += " tblCases.Sex, "
    SQLstring += " tblCases.DateBirth, "
    SQLstring += " tblCases.DateDeath, "
    SQLstring += " tblCases.PMInterval, "
    SQLstring += " tblCases.Age, "
    SQLstring += " tblCases.GestationAtDeliveryInDays, "
    SQLstring += " tblCases.ReferralCase, "
    SQLstring += " tlkPtht_Pathologists.PathologistsID, "
    SQLstring += " tlkPtht_Pathologists.PathologistsText, "
    SQLstring += " tlkRef_Referral.ReferralID, "
    SQLstring += " tlkRef_Referral.ReferralText, "
    SQLstring += " tlkSsn_Season.SeasonID, "
    SQLstring += " tlkSsn_Season.SeasonText "
    SQLstring += "FROM   (((tblCases "
    SQLstring += "LEFT JOIN tlkPtht_Pathologists "
    SQLstring += " ON tblCases.Pathologist_PthtID = tlkPtht_Pathologists.PathologistsID) "
    SQLstring += "LEFT JOIN tlkRef_Referral "
    SQLstring += " ON tblCases.ReferralCategory_RefID = tlkRef_Referral.ReferralID) "
    SQLstring += "LEFT JOIN tblAutopsies "
    SQLstring += " ON tblCases.CaseID = tblAutopsies.CaseID) "
    SQLstring += "LEFT JOIN tlkSsn_Season "
    SQLstring += " ON tblCases.Season_SsnID = tlkSsn_Season.SeasonID "
    SQLstring += "ORDER BY "
    SQLstring += " tblCases.CaseID "
    SQLstring += ";"

    res_crsr.execute(SQLstring)
    CaseRows = res_crsr.fetchall()

    row = 0
    first_time = time.time()
    last_time = time.time()

    print("Processing Cases")

    for CaseRow in CaseRows:

        row += 1

        #Get staff_id
        staff_id = GetStaffID(cnxn, crsr, staff_type_concept_id, str(CaseRow.PathologistsID), CaseRow.PathologistsText)

        #Get patient_id
        patient_id = GetPatientID(cnxn, crsr, CaseRow.HospitalNo, CaseRow.PMNumber, CaseRow.FirstName, CaseRow.Surname, CaseRow.Sex, CaseRow.DateBirth, CaseRow.DateDeath)

        #Add Age Category as a patient attribute
        AgeCategoryID = GetAgeCategoryID(CaseRow.GestationAtDeliveryInDays, CaseRow.Age, CaseRow.ReferralText)
        AgeCategory = GetAgeCategory(CaseRow.GestationAtDeliveryInDays, CaseRow.Age, CaseRow.ReferralText)

        AddPatientAttribute(cnxn, crsr, patient_id, "AC", "Age Category", "ID",AgeCategoryID, AgeCategory)
        #Add Age as a patient attribute - if exists, NB -999
        if not pandas.isnull(CaseRow.Age) and CaseRow.Age > 0:
            AddPatientAttribute(cnxn, crsr, patient_id, "AG", "Age in Days", "IN", CaseRow.Age)
        #Add Gestational Age as a patient attribute - if exists
        if not pandas.isnull(CaseRow.GestationAtDeliveryInDays) and CaseRow.GestationAtDeliveryInDays > 0:
            AddPatientAttribute(cnxn, crsr, patient_id, "GA", "Gestation At Delivery In Days", "IN",CaseRow.GestationAtDeliveryInDays)

        # Find Event datetime

        event_year = GetPMYear(CaseRow.PMNumber)

        if not pandas.isnull(CaseRow.DateAutopsy):
            event_start_date = CaseRow.DateAutopsy
        elif not pandas.isnull(CaseRow.DateDeath):
            if not pandas.isnull(CaseRow.PMInterval) and CaseRow.PMInterval > 0:
                event_start_date = CaseRow.DateDeath + datetime.timedelta(days=CaseRow.PMInterval)
            else:
                event_start_date = CaseRow.DateDeath
        else:
            if CaseRow.SeasonText == "Spring":
                event_start_date = datetime.datetime(event_year, 3, 1, 12, 0)
            elif CaseRow.SeasonText == "Summer":
                event_start_date = datetime.datetime(event_year, 6, 1, 12, 0)
            elif CaseRow.SeasonText == "Autumn":
                event_start_date = datetime.datetime(event_year, 9, 1, 12, 0)
            else:
                event_start_date = datetime.datetime(event_year, 12, 1, 12, 0)

        # Print(event_type_concept_id, lngStaff_id, lngPatient_id, datEvent_start_date, lngAgeCategory, strAgeCategory, intYear)

        #Get event_id
        event_id = GetEventID(cnxn, crsr, event_type_concept_id, patient_id, event_start_date, staff_id )

        #Add Event Attributes
        #Do we need some sort of code & alt code on Event?
        AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "CASEID", "Case ID", "IN", CaseRow.CaseID)
        AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "PMNumber", "PM Number", "TX", CaseRow.PMNumber)
        AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "Year", "Year", "IN", event_year)
        if not pandas.isnull(CaseRow.PMInterval) and CaseRow.PMInterval > 0:
            AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "PMINT", "PM Interval", "IN", CaseRow.PMInterval)
        if CaseRow.ReferralID > 0:
            AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "REF", "Referral", "ID", CaseRow.ReferralID, CaseRow.ReferralText)
        if CaseRow.SeasonID > 0:
            AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "SSN", "Season", "ID", CaseRow.SeasonID, CaseRow.SeasonText)
        if not pandas.isnull(CaseRow.DateAutopsy):
            AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblAutopsies", "AD", "Autopsy Date", "DT", CaseRow.DateAutopsy)

        CreateEventAttributes(cnxn, crsr, res_crsr, ReportTableRows, ReportTableFieldRows, CaseRow.CaseID, event_id, "Observation/PostMortem")

        print(row, CaseRow.PMNumber, CaseRow.SeasonText, f'{time.time() - last_time:.1f} case secs', f'{time.time() - first_time:.1f} total secs')
        gbl_add_event_time += (time.time() - last_time)
        last_time = time.time()

        if row >= max_rows:
            break

def CreateEventAttributes(cnxn, crsr, res_crsr, ReportTableRows, ReportTableFieldRows, CaseID, event_id, event_type_label):
    '''

    :param cnxn:
    :param crsr:
    :param ReportTableRows:
    :param CaseID:
    :param event_id:
    :param event_type_label:
    :return:
    '''

    global gbl_add_att_time

    for ReportTableRow in ReportTableRows:

        # print("Outputing Table: %s" % (ReportTableRow.SystemTableName))

        if ReportTableFieldRows:

            SQLstring = BuildEventsSQL(cnxn, crsr, ReportTableRow.ReportTableID, ReportTableRow.SystemTableID, ReportTableRow.SystemTableName, CaseID)
            res_crsr.execute(SQLstring)
            # Get list of column names for reference below
            ReportOutput_col_names = [x[0] for x in res_crsr.description]

            # Only one record per Case ID
            ReportOutputRow = res_crsr.fetchone()

            if ReportOutputRow:

                for ReportTableFieldRow in ReportTableFieldRows:

                    if ReportTableFieldRow.ReportTableID == ReportTableRow.ReportTableID:

                        if gbl_add_profiling:
                            start_time_att = time.time()

                        # print("Checking Field: %s" % (ReportTableFieldRow.SystemFieldName))

                        ReportOutputRow_dict = dict(zip(ReportOutput_col_names, ReportOutputRow))

                        value_type = "NL"
                        value = ReportOutputRow_dict[ReportTableFieldRow.SystemFieldName]

                        if not pandas.isnull(value):

                            # print("Adding field: %s Value: %s of Access type: %s, python type: %s" % (ReportTableFieldRow.SystemFieldName, value, ReportTableFieldRow.SystemFieldType, type(value)))

                            if ReportTableFieldRow.SystemFieldType == 1: #Boolean

                                value_type = "TF"
                                if value:
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "TF", 1)
                                else:
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "TF", 0)

                            elif ReportTableFieldRow.SystemFieldType == 3:  # Integer

                                if value != -999:
                                    value_type = "IN"
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "IN", value)

                            elif ReportTableFieldRow.SystemFieldType == 4:  # Long Integer
                                if not pandas.isnull(ReportTableFieldRow.SystemLKTableID) and ReportTableFieldRow.SystemLKTableID > 0 and "CaseID:AutopsyID:InternalExamID:RadiologyID".find(ReportTableFieldRow.SystemFieldName) == -1:
                                    if value > 0:
                                        # Get Lookup table name
                                        if not pandas.isnull(ReportTableFieldRow.SystemLKTableName):
                                            LKTableName = ReportTableFieldRow.SystemLKTableName
                                            LKIDFieldName = LKTableName[LKTableName.find("_") + 1:] + "ID"
                                            LKTextFieldName = LKTableName[LKTableName.find("_") + 1:] + "Text"
                                            # Get look up text for Concept Label
                                            row = res_crsr.execute("SELECT %s FROM %s WHERE %s = %s" % (LKTextFieldName, LKTableName, LKIDFieldName, value)).fetchone()
                                            if not pandas.isnull(row):
                                                value_type = "ID"
                                                AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "ID", value, row[0])
                                            else:
                                                pass #TODO Add message for missing value
                                        else:
                                            pass #TODO Add message for missing lookup table
                                else:
                                    if value != -999:
                                        value_type = "IN"
                                        AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "IN", value)

                            elif ReportTableFieldRow.SystemFieldType == 6 or ReportTableFieldRow.SystemFieldType == 7:  # Single or Double

                                if value == -999:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-6:] == "Weight" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-6:] == "Length" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-5:] == "Width" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-5:] == "Depth" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-12:] == "WeightNumber" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-12:] == "LengthNumber" and value <= 0:
                                    pass
                                elif ReportTableFieldRow.SystemFieldName[-13:] == "Circumference" and value <= 0:
                                    pass
                                else:
                                    value_type = "FL"
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "FL", value)

                            elif ReportTableFieldRow.SystemFieldType == 8:  # Date

                                #Need to split Date, DateTime and Time, what about Time of Day and Time to do something (start and end datetimes?
                                #I am using a date time with a value_type string so add TD for time of day and TM for time to do smothing but only measures up to 24 hours.
                                if value.date() <= datetime.date(1900, 1, 1):
                                    value_type = "TD"
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "TD", value)
                                elif value.time() <= datetime.time(0, 0):
                                    value_type = "DA"
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "DA", value)
                                else:
                                    value_type = "DT"
                                    AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "DT", value)

                            elif ReportTableFieldRow.SystemFieldType == 10:  # Text

                                value_type = "TX"
                                AddEventAttribute(cnxn, crsr, event_id, event_type_label + "/" + ReportTableRow.SystemTableName, ReportTableFieldRow.SystemFieldName, ReportTableFieldRow.SystemFieldName, "TX", value)

                            else:
                                value_type = "ER"
                                print("*** Error ***")

                        if gbl_add_profiling:
                            end_time_att = time.time()
                            gbl_add_att_time += (end_time_att - start_time_att)
                            # print(event_id, value_type, f' - Add Event Attribute: {end_time_att - start_time_att:.4f} secs')

    '''
    '''
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

    create_has_tables(rep_cnxn, rep_crsr)

    # runTests(rep_cnxn, rep_crsr)

    CreateEvents(rep_cnxn, rep_crsr, res_crsr, 3000)

    if gbl_add_profiling:
        print(gbl_add_event_time, gbl_add_att_time)

    CreateCOD2_SUMMAttributeFromCOD2Attribute(rep_cnxn, rep_crsr)

    # only for Post Mortem Events
    CreateAttributeNoOfAttributes(rep_cnxn, rep_crsr)

    # CreateLabEvents(rep_cnxn, rep_crsr)

    create_reporting_attributes(rep_cnxn, rep_crsr)

    # destination_folder = "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\"

    # CreateHASCSVFiles(rep_cnxn, rep_crsr, destination_folder)

    rep_cnxn.close()
    res_cnxn.close()


if __name__ == "__main__":
    main()

