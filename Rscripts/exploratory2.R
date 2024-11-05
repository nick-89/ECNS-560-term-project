library(tidyverse)
library(ggplot2)
options(scipen = 999)
getwd
theme_set(theme_bw())
data1 = read.csv("acs_cacensus.csv")
#the shelter cost data is relatively top skewed so a log transformation makes sense and it gives the data a more normal
#distribution.
pshelter = ggplot(data = data1,aes(x=shelter_cost))
pshelter + geom_density()
p_log_shelter = ggplot(data = data1,aes(x=log(shelter_cost)))
p_log_shelter+geom_density()
#group by state and province and show correlation between mean of total income and shelter cost
data2 = data1 %>% 
  group_by(state.province) %>% 
  summarise(across(c("inctot","shelter_cost"),~mean(.x,na.rm =T)))
p_shelter_state = ggplot(data2,aes(x=inctot,y=shelter_cost))
p_shelter_state+
  geom_point(aes(col = state.province),size = 2)+
  geom_smooth(method = "loess")+
  labs(title = "Mean Home Cost and Income by State")+
  xlab("Total Yearly Income")+
  ylab("Monthly Shelter Costs")+
  guides(color = "none")
#density plot of shelter cost based on country
p_countrycost = ggplot(data1,aes(log(shelter_cost)))
p_countrycost+geom_density(aes(fill = country),alpha=.8,adjust = 2)+
  labs(title = "Shelter Cost by Country")+
  xlab("Shelter Cost log transformed")
#citizenship and housing costs
data3=data1
data3$ownership[data3$ownership == 1] = "Owned"
data3$ownership[data3$ownership == 2] = "Rented"
data3$ownership[data3$ownership == 0] = "Unknown"
citcost = ggplot(data3,aes(log(shelter_cost)))
citcost+geom_density(aes(fill = factor(ownership)),alpha=.8,adjust = 2)+
  labs(title = "Shelter Cost by ownership")+
  xlab("Log Transformed Shelter Cost")+
  guides(fill=guide_legend("Ownership"))

