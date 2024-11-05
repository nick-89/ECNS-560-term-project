library(tidyverse)
library(ipumsr)
library(readxl)
library(rvest)
#----
census2021=read.csv("2021canadacensus/data_donnees_2021_ind_v2.csv")
#filtering out respondents younger than 18
census2 = census2021 %>% 
  filter(AGEGRP>6)
#Here I am removing variables that are of no use to me and do not have any corresponding
#variables in my other data so that merging them is easier. There may have been quicker ways
#to do this, but I didn't want to accidentally leave something out and not notice until too late.
census3 = census2 %>% 
  select(!c(
    REPAIR,NOS,HCORENEED_IND,PRIHM,HHTYPE,PKIDS,PKID25,PKID15_24,PKID6_14,PKID2_5,PKID0_1,EfSize,
    CFSTAT,CfSize,SempI,Retir,OtInc,OASGI,LoMBM_2018,LoLIMB,LoLIMA,LICO_BT,LICO_AT,Invst,IncTax,
    HHMRKINC,HHInc_AT,GTRfs,GovtI,EmpIn,EICBN,EFInc_AT,EFInc,EfDIMBM_2018,EFDecile,CQPPB,COVID_ERB,
    ChldC,CHDBN,CFInc_AT,CFInc,CapGn,PWPR,PWOCC,PWLEAVE,PWDUR,POWST,MODE,DIST,WRKACT,WKSWRK,NOC21,
    NAICS,LSTWRK,LFACT,JOBPERM,FPTWK,COW,SSGRAD,LOCSTUD,LOC_ST_RES,ATTSCH,CIP2021_STEM_SUM,CIP2021,
    POBPAR2,POBPAR1,POB,IMMSTAT,IMMCAT5,GENSTAT,CitOth,AGEIMM,NOL,MTNNO,MTNFR,MTNEN,LWREGNO,
    LWREGEN,LWREGFR,LWMOSTNO,LWMOSTEN,LWMOSTFR,LIPROGTYPE,KOL,HLREGNO,HLREGEN,HLREGFR,HLMOSTEN,HLMOSTFR,
    HLMOSTNO,FOL,Relig,VISMIN,ETHDER,REGIND,BFNMEMB,ABOID,MarStH,BedRm,CONDO,DTYPE,LI_ELIG_OML_U18,
    MrkInc,ROOM,Subsidy,TotInc_AT,YRIM,HHInc, PresMortG,PR1,PR5,Mob5
  ))
census4 = census3 %>% 
  select(!c(WT1:WT16))

#I am only comparing household sizes of 1 in order to make this more of a study at the individual level instead of
#household level in order for the data to be more workable and interpretable
census5 = census4 %>% 
  filter(HHSIZE==1)

#renaming and removing redundant variables for consistency
census6 = census5 %>% 
  rename(metropolitan_area=CMA) %>% 
  rename(race=DPGRSUM) %>% 
  rename(sex=Gender) %>% 
  rename(migrate1 = Mob1) %>% 
  rename(INCTOT = TotInc) %>% 
  rename(INCWAGE = Wages) %>% 
  rename(shelter_cost = SHELCO) %>% 
  rename(state_province=PR) %>% 
  rename(citizen=Citizen) %>% 
  rename(ownership = Tenur) %>% 
  rename(house_value = Value) %>% 
  rename(age = AGEGRP) %>% 
  rename(serial = PPSORT) %>% 
  rename(education = HDGREE)
  
census7=census6 %>% 
  select(!HHSIZE)
  
names(census7)=tolower(names(census7))

#replacing not in universe codes with NAs
census7$house_value[census7$house_value==99999999] = "NA"
census7$incwage[census7$incwage==99999999] = "NA"
census7$inctot[census7$inctot==99999999] = "NA"
census7$house_value[census7$house_value==88888888] = "NA"
census7$incwage[census7$incwage==88888888] = "NA"
census7$inctot[census7$inctot==88888888] = "NA"
census7$house_value=as.numeric(census7$house_value)
census7$incwage=as.numeric(census7$incwage)
census7$inctot=as.numeric(census7$inctot)
#This section is all about making the variable codes between the ACS and canadian survey comparable.
#Changing race, sex, education, and mobility codes in order to be consistent with the ACS
census7$race[census7$race == 1] = "01"
census7$race[census7$race == 3] = "04"
census7$race[census7$race == 4] = "02"
census7$race[census7$race == 5] = "06"
census7$race[census7$race == 8] = "06"
census7$race[census7$race == 9] = "06"
census7$race[census7$race == 10] = "06"
census7$race[census7$race == 11] = "05"
census7$race[census7$race == 12] = "07"
census7$race[census7$race == 13] = "07"
census7$race[census7$race == 14] = "03"
census7$race[census7$race == 2] = "06"
census7$race[census7$race == 6] = "07"
census7$race[census7$race == 7] = "01"
census7$race[census7$race == 88] = "NA"
census7$race=as.numeric(census7$race)

census7$sex[census7$sex == 2] = "01"
census7$sex[census7$sex == 1] = "02"

census7$sex=as.numeric(census7$sex)

census7$education[census7$education == 13] = "116"
census7$education[census7$education == 12] = "114"
census7$education[census7$education == 11] = "115"
census7$education[census7$education == 10] = "115"
census7$education[census7$education == 9] = "101"
census7$education[census7$education == 8] = "081"
census7$education[census7$education == 7] = "081"
census7$education[census7$education == 6] = "081"
census7$education[census7$education == 5] = "081"
census7$education[census7$education == 4] = "081"
census7$education[census7$education == 3] = "081"
census7$education[census7$education == 2] = "063"
census7$education[census7$education == 1] = "061"
census7$education=as.numeric(census7$education)

census7$migrate1[census7$migrate1 ==3] = "2"
census7$migrate1[census7$migrate1 == 4] = "2"
census7$migrate1[census7$migrate1 == 5] = "3"
census7$migrate1[census7$migrate1 == 6] = "4"
census7$migrate1[census7$migrate1 == 8] = "9"
census7$migrate1=as.numeric(census7$migrate1)




census7$ownership[census7$ownership == 8] = "0"

#All currency will be represented using 2021 USD and the conversion I got will be cited.
census7$inctot=census7$inctot*.7978
census7$incwage=census7$incwage*.7978
census7$shelter_cost=census7$shelter_cost*.7978
census7$house_value=census7$house_value*.7978

census7$ownership=as.numeric(census7$ownership)

#Web scraping a bridge data set to convert province idnetifiers from code to names using rvest
url = "https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8"
page=read_html(url)
tables=page %>% 
  html_elements("table")
tables[[1]]
provincebridge=tables[[1]] %>% 
  html_table()
#Cleaning this up by removing white spaces in the new data
names(provincebridge)=make.names(names(provincebridge),unique = TRUE)
provincebridge$Province.Territory=make.names(provincebridge$Province.Territory,unique = TRUE)
provincebridge2=provincebridge %>% 
  rename(state_province=Standard.geographical.classification..SGC..code)
provincebridge3=provincebridge2 %>% 
  select(c(Province.Territory,state_province))
provincebridge3$state_province=as.numeric(provincebridge3$state_province)
#joining the two data sets in order to actually swap names properly
censusprovince=left_join(census7,provincebridge3,by="state_province")
censusprovince2=censusprovince %>% 
  rename(state.province=Province.Territory) %>% 
  select(!state_province)

countrycan=rep("Can",length(censusprovince2$serial))
censusprovince2$country=countrycan

#----
ddi <- read_ipums_ddi("usa_00004.xml")
usa <- read_ipums_micro(ddi)
#Here I am getting rid of data where key variables are not in universe or actually = 0
usa2=usa
usa2$OWNCOST = usa2$OWNCOST%>% 
  str_replace_all("99999","NA")
usa2$COSTELEC = usa2$COSTELEC%>% 
  str_replace_all("9997","0")
usa2$COSTELEC = usa2$COSTELEC%>% 
  str_replace_all("9993","0")
usa2$COSTGAS = usa2$COSTGAS%>% 
  str_replace_all("9997","0")
usa2$COSTGAS = usa2$COSTGAS%>% 
  str_replace_all("9993","0")
usa2$COSTWATR = usa2$COSTWATR%>% 
  str_replace_all("9997","0")
usa2$COSTWATR = usa2$COSTWATR%>% 
  str_replace_all("9993","0")
usa2$COSTFUEL = usa2$COSTFUEL%>% 
  str_replace_all("9997","0")
usa2$COSTFUEL = usa2$COSTFUEL%>% 
  str_replace_all("9993","0")

#removing labels (they mess stuff up for me sometimes)
usa3=zap_ipums_attributes(usa2)

usa3$OWNCOST=as.numeric(usa3$OWNCOST)
usa3$COSTELEC = as.numeric(usa3$COSTELEC)
usa3$COSTWATR = as.numeric(usa3$COSTWATR)
usa3$COSTFUEL = as.numeric(usa3$COSTFUEL)
usa3$COSTGAS = as.numeric(usa3$COSTGAS)

#making variables monthly values
usa3$COSTELEC=usa3$COSTELEC/12
usa3$COSTWATR=usa3$COSTWATR/12
usa3$COSTFUEL=usa3$COSTFUEL/12
usa3$COSTGAS=usa3$COSTGAS/12

#creating a shelter cost that is comparable to the canadian census variable and then removing redundant variables
usa4 = usa3 %>% 
  mutate(shelter_cost=OWNCOST+RENTGRS+CONDOFEE+COSTELEC+COSTGAS+COSTWATR+COSTFUEL)
usa5=usa4 %>% 
  select(!c(OWNCOST,RENTGRS,CONDOFEE,COSTELEC,COSTGAS,COSTWATR,COSTFUEL,MOBLHOME,PROPTX99))
#renaming variables for cinsistency
usa6=usa5 %>% 
  rename(state_province=STATEFIP) %>% 
  rename(metropolitan_area=MET2013) %>% 
  rename(household_income=HHINCOME) %>% 
  rename(house_value = VALUEH) %>% 
  rename(employment_status=EMPSTAT) %>% 
  rename(WEIGHT = HHWT) %>% 
  rename(citizen = CITIZEN) %>% 
  rename(race=RACE) %>% 
  rename(sex=SEX) %>% 
  rename(migrate1=MIGRATE1) %>% 
  rename(ownership = OWNERSHP) %>% 
  rename(education = EDUCD)

#filtering to household size of one
usa7=usa6 %>% 
  filter(FAMSIZE==1,PERNUM==1,NFAMS==1)
usa8=usa7 %>% 
  select(!c(OWNERSHPD,NFAMS,PERNUM,FAMSIZE,RACED,EDUC,EMPSTATD,CLASSWKR,
            CLASSWKRD,OCC,YEAR,SAMPLE,STRATA,CLUSTER,COUNTYICP,GQ,PERWT,household_income,
            REGION,employment_status,CBSERIAL,MIGRATE1D
            ))
#Adding the highest number in the unique identifier of the canadian data to ensure there's no overlap
#in my primary key
usa8$SERIAL=usa8$SERIAL+980868
names(usa8)=tolower(names(usa8))

usa8$citizen = as.numeric(str_replace_all(usa8$citizen,"0","1"))

usa8$race[usa8$race == 9] = "7"
usa8$race[usa8$race == 8] = "7"
usa8$race = as.numeric(usa8$race)

#The canadian census groups ages so I had to create bins in this one
#even though I would prefer using each age as a variable
usa9 = usa8 %>% 
  mutate(agebin = cut(age,breaks = c(
    0,17,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200
    ),labels = FALSE)) %>% 
  select(!age)
usa9 = usa9 %>% 
  rename(age = agebin)
#making the bin codes consistent with the canadian data
usa9$age = usa9$age+5
#Converting the state identifiers from fips code to state name using a bridge data set and joining them
fipsbridge = read_excel("Fipsbridge.xlsx")
fipsbridge2=fipsbridge %>% 
  rename(state_province=statefip)

usastate=left_join(usa9,fipsbridge2,by = "state_province")

usastate2=usastate %>% 
  rename(state.province=state) %>% 
  select(!state_province)

census7$migrate1[census7$migrate1 ==3] = "2"
usastate2$house_value[usastate2$house_value==9999999] = "NA"
usastate2$house_value=as.numeric(usastate2$house_value)
countryusa=rep("USA",length(usastate2$serial))
usastate2$country=countryusa
#----
str(census7)
str(usa9)
#I use the bind rows function to combine the datasets instead of a join because the two data sets contain
#the same set of variables that have been modified accordingly to be comparable with each other
combineddata=bind_rows(usastate2,censusprovince2)
#ensuring I didn't lose any rows
length(usastate2$serial)+length(censusprovince2$serial)-length(combineddata$serial)
write.csv(combineddata,"acs_cacensus.csv")
combined=read.csv("acs_cacensus.csv")

#I realize that a lot of my code is super inefficient. Looking back I would have done much of it differently,
#but what's done is done