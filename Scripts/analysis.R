library(tidyverse)
library(broom)
library(car)
library(stargazer)
data1=read.csv("acs_cacensus2.csv")
data2 = data1 %>%
  mutate(across(where(is.integer), as.factor))
data2$bedrooms=as.numeric(data2$bedrooms)
data3 = data2 %>%
  select(!X) %>%
  mutate(
    log_total_income = log(total_income + 1),
    log_employment_income = log(employment_income + 1),
    log_investment_income = log(investment_income + 1),
    log_house_value = log(house_value + 1),
    log_shelter_cost = log(shelter_cost + 1)
  )

data4 = data3 %>%
  na.omit()
model1 = lm(log_shelter_cost ~ 
               log_total_income + 
               log_house_value + 
               log_employment_income + 
               log_investment_income + 
               bedrooms + 
               marital_status + 
               age + 
               level_of_educ +
               labor_force_status +
               state.province +
               ownership +
               sex,
             data = data3)
summary_model1 = tidy(model1)


stargazer(model1,type = "html",
          dep.var.labels = "Log Shelter Costs",  title="Results",
          font.size="small", keep=c(1:6),11)
#--------
