data1=read.csv("acs_cacensus2.csv")
library(tidyverse)
library(ggplot2)
library(viridis)
library(corrplot)
data2 = data1 %>%
  mutate(across(where(is.integer), as.factor))
str(data2)
data3 = data2 %>%
  mutate(log_shelter_cost = log(as.numeric(as.character(shelter_cost))))

p0 = ggplot(data3, aes(x = log_shelter_cost,fill = country))+geom_density(alpha=.5)+
  labs(title = "Density Plot of Log Shelter Cost by Country",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_bw()
p0
ggsave(filename = "densitycountry.png",dpi = 300)

p1 = ggplot(data3, aes(x = log_shelter_cost)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Basic Density Plot of Log Shelter Cost",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_bw()
p1
ggsave(filename = "density1.png",dpi = 300)

dataown=data3 %>%
  filter(!is.na(ownership))%>%
  mutate(ownership=factor(ownership,levels = c("1","2"),
                          labels = c("Owned","Rented")))
  
p2 = ggplot(dataown, aes(x = log_shelter_cost, fill = ownership)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Log Shelter Cost by Tenure Status",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal()
p2
ggsave(filename = "own.png", dpi = 300, width = 10, height = 6)

data5=data3 %>%
  mutate(mortgage=factor(mortgage,levels = c("0","1"),
                          labels = c("No Mortgage","Mortgage")))
p3 = ggplot(data5, aes(x = log_shelter_cost, fill = mortgage)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Log Shelter Cost by Mortgage Status",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal()
p3
ggsave(filename = "mortgage.png", dpi = 300, width = 10, height = 6)

data6=data3 %>%
  filter(rooms!=88)
p4 = ggplot(data6, aes(x = log_shelter_cost, fill = rooms)) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Density of Log Shelter Cost by Number of Rooms",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal()+
  theme(
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.position = "right"
  ) +guides(fill = guide_legend(ncol = 1))
p4
ggsave(filename = "rooms.png",dpi = 300)

numeric_data = data2 %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  select(where(is.numeric))

cor_matrix = cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.45,
         tl.cex = 0.5,
         diag = FALSE,
         col = colorRampPalette(c("skyblue", "white", "pink"))(200),
         title = "Correlation Matrix of Variables",
         mar = c(0,0,1,0))
ggsave(filename = "corrplot.png", dpi = 300, width = 10, height = 6)

data4 = data3 %>% 
  group_by(state.province) %>% 
  summarise(across(c("total_income","shelter_cost"),~median(.x,na.rm =T)))

p_shelter_state = ggplot(data4,aes(x=log(total_income),y=log(shelter_cost)))

p_shelter_state +
  geom_point(aes(col = state.province), size = 2) +
  geom_smooth(method = "lm") +
  labs(title = "Mean Home Cost and Income by State") +
  xlab("Log Income") +
  ylab("Log Shelter Costs") +
  theme(
    legend.key.size = unit(0.3, "cm"),  # Makes the legend keys smaller
    legend.text = element_text(size = 6),  # Reduces legend text size
    legend.title = element_text(size = 8),  # Reduces legend title size
    legend.position = "right"  # Keeps legend on the right side
  )
ggsave(filename = "state.png", dpi = 300, width = 10, height = 6)

data_gender = data3 %>%
  mutate(gender = factor(sex, 
                         levels = c("1", "2"),
                         labels = c("Male", "Female")))

p_gender = ggplot(data_gender, aes(x = log_shelter_cost, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Log Shelter Cost by Gender",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 8))
p_gender
ggsave(filename = "gender_density.png", dpi = 300, width = 10, height = 6)

data_citizen = data3 %>%
  mutate(citizen = factor(citizen,
                          levels = c("1", "2"),
                          labels = c("Citizen", "Not a Citizen")))

p_citizen = ggplot(data_citizen, aes(x = log_shelter_cost, fill = citizen)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Log Shelter Cost by Citizenship Status",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(ncol = 1))
p_citizen
ggsave(filename = "citizen_density.png", dpi = 300, width = 10, height = 6)

data_race = data3 %>%
  mutate(race = factor(race,
                       levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                       labels = c("White", "Black", "American Indian", "Chinese", 
                                  "Japanese", "Other Asian", "Other Race", 
                                  "Two Major Races", "Three+ Major Races")))

p_race = ggplot(data_race, aes(x = log_shelter_cost, fill = race)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Log Shelter Cost by Race",
       x = "Log Shelter Cost",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_viridis_d(option = "plasma")  # Using turbo palette for better distinction between categories
p_race
ggsave(filename = "race_density.png", dpi = 300, width = 10, height = 6)




