# MSc Project
Project Title: Identifying Feature Importance in Pediatric Post-mortem Outcome with Machine Learning Models  

## MSc Project code - Post-mortem Research Database

### Python Scripts:
* create_has_tables.py: Python script to create and populate HAS tables
* create_rdvs.py: Python script to create RDV CSV and XML files
* modify_csv_data.py: Python script to modify original RDV files creates revised CSV and XML file
* modify_events.py: Python script to add new event attributes to existing events

### Other Folders
* RCode: R Scripts produced during the project 
* Data: RDV CSV and XML files used to produced output for the project report
* Docs: Project report files
* Docs\Images: Images files used in project report
* Docs\Images\run_01: Support files output by R scripts produced during a complete run and used in the project report


### Create_HAS_Tables.py
#### create_has_tables
* DROPs and CREATEs HAS Tables in Staging database
* Creates base concepts
#### runTests
Completes unit testing on the following procedures and functions:
* GetConceptID
* GetStaffID
* GetPatientID
* GetEventID
* GetEventAttributeID
* AddPatientAttribute
* AddEventAttribute
* BuildEventsSQL
* GetAgeCategory
* GetPMYear

#### CreateEvents
#### CreateAttributeFromAttributes
#### CreateAttributeNoOfAttributes
#### CreateLabEvents
#### create_reporting_attributes
#### CreateHASCSVFiles

