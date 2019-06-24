import pyodbc

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

rep_cnxn.close()
