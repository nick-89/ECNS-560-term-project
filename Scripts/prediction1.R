library(tidyverse)
library(tidymodels)
library(ipumsr)
library(readxl)
library(rvest)

#-----
census2021=read.csv("rawdata/2021canadacensus/data_donnees_2021_ind_v2.csv")
ddi <- read_ipums_ddi("usa_00005.xml")
acs <- read_ipums_micro(ddi)
#variables
canada=census2021 %>% 
  select(c(AGEGRP,Gender,MarStH,Mob1,DPGRSUM,Citizen,ATTSCH,
           HDGREE,COW,LFACT,WKSWRK,PWDUR,CQPPB,EmpIn,Invst,
           TotInc,PRIHM,BedRm,PresMortG,ROOM,SHELCO,Tenur,Value,PR,CMA,PPSORT
           ))
usa = acs %>% 
  select(!c(GQ,OWNERSHPD,RELATED,RACED,BPLD,TRIBED,EDUC,DEGFIELDD,CLASSWKRD,FTOTINC,MIGRATE1D,BPL,
            YRIMMIG,TRIBE,DEGFIELD,OCC,IND1950,UHRSWORK,MIGPLAC1,PWSTATE2,TRANWORK,RIDERS,INCRETIR))

#creating shelter cost usa
usa2 = usa %>%
  mutate(across(c(COSTELEC, COSTGAS, COSTWATR), 
                ~ifelse(. %in% c(9997, 9993), 0, .)))
usa3 = usa2 %>%
  mutate(OWNCOST = ifelse(OWNCOST==99999,0,OWNCOST))
usa4 = usa3 %>% 
  mutate(across(c(COSTELEC, COSTGAS, COSTWATR),
                ~./12))
usa5=usa4 %>% 
  mutate(across(c(MORTAMT1,OWNCOST),
                ~ifelse(. %in% 4,0,.)))
usa6 = usa5 %>% 
  mutate(shelter_cost=MORTAMT1+MORTAMT2+OWNCOST+RENTGRS+CONDOFEE+COSTELEC+COSTGAS+COSTWATR, .after = COSTWATR)

usa7=usa6 %>% 
  mutate(VALUEH=ifelse(VALUEH==9999999,0,VALUEH))
#Metropolitan area
canada2=canada %>% 
  mutate(CMA=ifelse(CMA<999,2,1))

usa8=usa7 %>% 
  mutate(METRO=ifelse(METRO>2,2,METRO))
#tenure
canada3=canada2 %>% 
  mutate(Tenur=ifelse(Tenur==8,NA,Tenur))
usa9=usa8 %>% 
  mutate(OWNERSHP=ifelse(OWNERSHP==0,NA,OWNERSHP))
#mortgage
canada4=canada3 %>% 
  mutate(PresMortG=ifelse(PresMortG==1,1,0))
usa10=usa9 %>% 
  mutate(MORTGAGE=ifelse(MORTGAGE>1,1,0))
#primary maintainer
usa11=usa10 %>% 
  mutate(RELATE=ifelse(RELATE==1,1,0))
canada5=canada4 %>% 
  mutate(PRIHM=ifelse(PRIHM==1,1,0))

#gender
usa12=usa11 %>% 
  mutate(SEX=ifelse(SEX==2,1,2))
#Age
usa13=usa12 %>% 
  mutate(AGE = cut(AGE,breaks = c(
    0,17,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200
  ),labels = FALSE))
usa14=usa13 %>% 
  filter(AGE!=1)
usa14$AGE = usa14$AGE+5
canada6=canada5 %>% 
  filter(AGEGRP>6)
canada6=canada6 %>% 
  filter(AGEGRP!=88)
#marital status
usa15=usa14 %>% 
  mutate(MARST=ifelse(MARST %in% c(1,2),2,1))
canada7=canada6 %>% 
  mutate(MarStH=ifelse(MarStH==2,2,1))
#Race
usa16=usa15 %>% 
  mutate(RACE=ifelse(RACE %in% c(8,9),7,RACE))
canada7=canada7 %>% 
  rename(race=DPGRSUM)
canada7$race[canada7$race == 1] = "01"
canada7$race[canada7$race == 3] = "04"
canada7$race[canada7$race == 4] = "02"
canada7$race[canada7$race == 5] = "06"
canada7$race[canada7$race == 8] = "06"
canada7$race[canada7$race == 9] = "06"
canada7$race[canada7$race == 10] = "06"
canada7$race[canada7$race == 11] = "05"
canada7$race[canada7$race == 12] = "07"
canada7$race[canada7$race == 13] = "07"
canada7$race[canada7$race == 14] = "03"
canada7$race[canada7$race == 2] = "06"
canada7$race[canada7$race == 6] = "07"
canada7$race[canada7$race == 7] = "01"
canada7$race[canada7$race == 88] = "NA"
canada7$race=as.numeric(canada7$race)

#citizen
usa17=usa16 %>% 
  mutate(CITIZEN=ifelse(CITIZEN %in% c(0,1,2),1,2))
canada8=canada7 %>% 
  mutate(Citizen=ifelse(Citizen %in% c(1,2),1,2))
#School
canada9=canada8 %>% 
  mutate(ATTSCH=ifelse(ATTSCH %in% c(2,3,4,5),2,ATTSCH))
canada10=canada9 %>% 
  mutate(ATTSCH=ifelse(ATTSCH==8,0,ATTSCH))
#educ
canada10$ATTSCH[canada10$ATTSCH == 13] = "116"
canada10$ATTSCH[canada10$ATTSCH == 12] = "114"
canada10$ATTSCH[canada10$ATTSCH == 11] = "115"
canada10$ATTSCH[canada10$ATTSCH == 10] = "115"
canada10$ATTSCH[canada10$ATTSCH == 9] = "101"
canada10$ATTSCH[canada10$ATTSCH == 8] = "081"
canada10$ATTSCH[canada10$ATTSCH == 7] = "081"
canada10$ATTSCH[canada10$ATTSCH == 6] = "081"
canada10$ATTSCH[canada10$ATTSCH == 5] = "081"
canada10$ATTSCH[canada10$ATTSCH == 4] = "081"
canada10$ATTSCH[canada10$ATTSCH == 3] = "081"
canada10$ATTSCH[canada10$ATTSCH == 2] = "063"
canada10$ATTSCH[canada10$ATTSCH == 1] = "061"
canada10$ATTSCH=as.numeric(canada10$ATTSCH)

#laborforce
canada11=canada10 %>% 
  mutate(LFACT=ifelse(LFACT %in% c(1:10),1,2))

#class worker
canada12=canada11 %>% 
  mutate(COW = ifelse(COW %in% c(3:6),1,2))
#mobility
canada13=canada12 %>% 
  mutate(Mob1= ifelse(Mob1 %in% c(2:4),2,Mob1))
canada14=canada13 %>% 
  mutate(Mob1=ifelse(Mob1==5,3,Mob1))
canada15=canada14 %>% 
  mutate(Mob1=ifelse(Mob1==6,4,Mob1))
canada16=canada15 %>% 
  mutate(Mob1=ifelse(Mob1==8,NA,Mob1))
usa18=usa17 %>% 
  mutate(MIGRATE1=ifelse(MIGRATE1==9,NA,MIGRATE1))

#transit
usa19=usa18 %>% 
  mutate(TRANTIME = cut(TRANTIME,breaks = c(
    0,15,30,45,60,1000),labels = FALSE))
usa20=usa19 %>% 
  mutate(TRANTIME=ifelse(is.na(TRANTIME),0,TRANTIME))
canada17=canada16 %>% 
  mutate(PWDUR=ifelse(PWDUR==9,0,PWDUR))
#money
canada18=canada17 %>% 
  mutate(across(c(EmpIn,Invst,TotInc,CQPPB,Value),
                ~ifelse(. %in% c(99999999,88888888),NA,.)))
canada19=canada18 %>% 
  mutate(across(c(EmpIn,Invst,TotInc,CQPPB,Value,SHELCO),
                ~.*.7978))
#names
canada20=canada19 %>% 
  rename(age=AGEGRP) %>% 
  rename(sex=Gender) %>% 
  rename(marital_status=MarStH) %>% 
  rename(mobility=Mob1) %>% 
  rename(school=ATTSCH) %>% 
  rename(level_of_educ=HDGREE) %>% 
  rename(class_of_worker=COW) %>% 
  rename(labor_force_status=LFACT) %>% 
  rename(weeks_worked=WKSWRK) %>% 
  rename(commute=PWDUR) %>% 
  rename(pension=CQPPB) %>% 
  rename(employment_income=EmpIn) %>% 
  rename(investment_income=Invst) %>% 
  rename(total_income=TotInc) %>% 
  rename(primary_maintain=PRIHM) %>% 
  rename(bedrooms=BedRm) %>% 
  rename(mortgage=PresMortG) %>% 
  rename(rooms=ROOM) %>% 
  rename(shelter_cost=SHELCO) %>% 
  rename(ownership=Tenur) %>% 
  rename(house_value=Value) %>% 
  rename(state_province=PR) %>% 
  rename(metro=CMA) %>% 
  rename(id=PPSORT) %>% 
  rename(citizen=Citizen)
usa21=usa20 %>% 
  select(!c(YEAR,MORTAMT1:COSTWATR,PERWT,DEPARTS))
usa22=usa21 %>% 
  rename(age=AGE) %>% 
  rename(sex=SEX) %>% 
  rename(marital_status=MARST) %>% 
  rename(mobility=MIGRATE1) %>% 
  rename(school=SCHOOL) %>% 
  rename(level_of_educ=EDUCD) %>% 
  rename(class_of_worker=CLASSWKR) %>% 
  rename(labor_force_status=LABFORCE) %>% 
  rename(weeks_worked=WKSWORK2) %>% 
  rename(commute=TRANTIME) %>% 
  rename(pension=INCSS) %>% 
  rename(employment_income=INCWAGE) %>% 
  rename(investment_income=INCINVST) %>% 
  rename(total_income=INCTOT) %>% 
  rename(primary_maintain=RELATE) %>% 
  rename(bedrooms=BEDROOMS) %>% 
  rename(mortgage=MORTGAGE) %>% 
  rename(rooms=ROOMS) %>% 
  rename(shelter_cost=shelter_cost) %>% 
  rename(ownership=OWNERSHP) %>% 
  rename(house_value=VALUEH) %>% 
  rename(state_province=STATEFIP) %>% 
  rename(metro=METRO) %>% 
  rename(id=SERIAL) %>% 
  rename(citizen=CITIZEN) %>% 
  rename(race=RACE)
usa23=usa22 %>% 
  filter(PERNUM==1)
usa24=usa23 %>% 
  select(!PERNUM)

#bridges
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
canadaprovince=left_join(canada20,provincebridge3,by="state_province")
canadaprovince2=canadaprovince %>% 
  rename(state.province=Province.Territory) %>% 
  select(!state_province)

fipsbridge = read_excel("Fipsbridge.xlsx")
fipsbridge2=fipsbridge %>% 
  rename(state_province=statefip)

usastate=left_join(usa24,fipsbridge2,by = "state_province")

usastate2=usastate %>% 
  rename(state.province=state) %>% 
  select(!state_province)
#factors
canadaprovince3 = canadaprovince2 %>%
  mutate(across(where(is.numeric) & 
                  !c(employment_income, investment_income, total_income, shelter_cost, house_value),
                as.factor))
canadaprovince3$country = rep("Canada",length(canadaprovince3$state.province))
str(canadaprovince3)
usastate3 <- usastate2 %>%
  mutate(across(where(is.numeric) & 
                  !c(employment_income, investment_income, total_income, shelter_cost, house_value),
                as.factor))
str(usastate3)
usastate3=zap_ipums_attributes(usastate3)
usastate3$country = rep("USA",length(usastate3$state.province))
combineddata=bind_rows(usastate3,canadaprovince3)
combineddata = combineddata %>%
  mutate(
    pension = as.numeric(as.character(pension)),
    weeks_worked = as.numeric(as.character(weeks_worked))
  )
combineddata2=combineddata %>%
  select(!id)
write.csv(combineddata2,file = "acs_cacensus2.csv")

