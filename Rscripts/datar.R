library(ipumsr)
set_ipums_api_key(Sys.getenv("ipums_api"))
extract_definition = define_extract_micro("usa","My extract for project",
  c("us2006a","us2011a","us2016a","us2021a"),
  c("YEAR","CPI99","OWNERSHIP","RENT","RENTGRS","HHINCOME",
    "VALUEH","NFAMS","FAMSIZE","SEX","AGE","RACE","CITIZEN",
    "EDUC","EMPSTAT","CLASSWKR","OCC","INCTOT","INCWAGE","MIGRATE1"))
save_extract_as_json(extract_definition,"ipumsusa.json")
save_extract_as_json()
ipumsr::define_extract_from_js