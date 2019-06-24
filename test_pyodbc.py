import pyodbc

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

drop_table(rep_cnxn, rep_crsr, "ha_concepts")

SQLstring = "CREATE TABLE ha_concepts ( "
SQLstring += "concept_id             AUTOINCREMENT PRIMARY KEY, "
SQLstring += "parent_concept_id      INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
SQLstring += "value_type_concept_id  INTEGER NOT NULL REFERENCES ha_concepts(concept_id) ON UPDATE CASCADE ON DELETE CASCADE, "
SQLstring += "concept_uri            VARCHAR(255) NULL, "
SQLstring += "code                   VARCHAR(255) NULL, "
SQLstring += "term                   VARCHAR(255) NULL, "
SQLstring += "label                  VARCHAR(255) NULL, "
# SQLstring += "note                   VARCHAR(255) NULL, "
SQLstring += "category               VARCHAR(255) NOT NULL, "
SQLstring += "created                DATETIME DEFAULT NOW() NOT NULL "
SQLstring += ");"
rep_crsr.execute(SQLstring)
rep_crsr.commit()

rep_cnxn.close()
