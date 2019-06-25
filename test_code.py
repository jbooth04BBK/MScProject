# AddEventAttribute(cnxn, crsr, event_id, "Observation/PostMortem/tblCases", "REF", "Referral", "ID", "1",
#                  "Sudden Death < 12 months")

event_type_category = "Observation/PostMortem/stuff/tblCases"

if event_type_category.find("/tbl") > 0:
    parent_category = "/EventAttribute/" + event_type_category[:event_type_category.find("/tbl")] + "/LookUp"
    print(parent_category)

parent_category = event_type_category.rsplit("/",1)[0]
parent_code = event_type_category.rsplit("/",1)[1]
print(parent_category)
print(parent_code)
