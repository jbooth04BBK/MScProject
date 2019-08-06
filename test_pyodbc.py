import pyodbc

import Create_HAS_Tables

'''
I should put some comments in here
'''
# Get list of drivers
for x in pyodbc.drivers():
    print(x)

rep_conn_str = (
    r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
    r'DBQ=I:\DRE\Projects\Research\0004-Post mortem-AccessDB\DataExtraction\PMResearchReportDB.accdb;Uid=Admin;Pwd=;ExtendedAnsiSQL=1;'
)
rep_cnxn = pyodbc.connect(rep_conn_str)
rep_crsr = rep_cnxn.cursor()

for table_info in rep_crsr.tables(tableType='TABLE'):
    print(table_info)

# Create_HAS_Tables.drop_table(rep_cnxn, rep_crsr, "ha_concepts")
#
# SQLstring = "CREATE TABLE ha_concepts ( "
# SQLstring += "concept_id             AUTOINCREMENT PRIMARY KEY, "
# SQLstring += "parent_concept_id      INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
# SQLstring += "value_type_concept_id  INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
# SQLstring += "concept_uri            VARCHAR(255) NULL, "
# SQLstring += "code                   VARCHAR(255) NULL, "
# SQLstring += "term                   VARCHAR(255) NULL, "
# SQLstring += "label                  VARCHAR(255) NULL, "
# # SQLstring += "note                   VARCHAR(255) NULL, "
# SQLstring += "category               VARCHAR(255) NOT NULL, "
# SQLstring += "created                DATETIME DEFAULT NOW() NOT NULL "
# SQLstring += ");"
# rep_crsr.execute(SQLstring)
# rep_crsr.commit()

# Add NOT NULL constraint to column

# SQLstring = "ALTER TABLE ha_concepts "
# SQLstring += "  ALTER COLUMN parent_concept_id INTEGER NOT NULL"
# SQLstring += ";"
#
# rep_crsr.execute(SQLstring)
# rep_cnxn.commit()
#
# SQLstring = "ALTER TABLE ha_concepts "
# SQLstring += "  ALTER COLUMN value_type_concept_id INTEGER NOT NULL"
# SQLstring += ";"
#
# rep_crsr.execute(SQLstring)
# rep_cnxn.commit()

'''
3448	552000
725	500000
1907	144000
5024	134500 ** Not sure about this one **
849	126000
KG values
Age, gest Age, caseid, body_weight
5173		4009	100.5999
4259		2915	75.4
4899		6877	71
5599		3686	70.5
5337		6451	64
3515		3837	53

9449		6515	51

5590		6875	49
4549		1175	48
5289		2416	47.7999
5107		1332	46.5
5105		2628	43.0999
3373		146	36.2
1935	280	2896	22.8999
1857		6343	19
1432		664	18.3999
1679		6299	18
1068		6482	17
867	203	1189	15
951		6863	15
435		3425	12.5
407		2568	10.6
1220	266	1112	10
454	161	2641	7.1999
776	280	2569	7.0999

'''

Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr, 3448, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 55200.0000)
Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr,  725, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 50000.0000)
Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr, 1907, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 14400.0000)
Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr, 5024, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 13450.0000)
Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr,  849, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 12600.0000)

Create_HAS_Tables.update_event_attribute_value(rep_cnxn, rep_crsr, 6515, "/EventAttribute/Observation/PostMortem/tblExternalExams", "BodyWeight", 51000.0000)

rep_cnxn.close()
