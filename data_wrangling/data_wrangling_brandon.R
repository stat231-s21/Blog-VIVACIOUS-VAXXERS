library(readr)
library(tidyverse)
library(DT)
library(mosaic)
library(mosaic)
Health_Insurance <- read_csv("data/Indicators_of_Health_Insurance_Coverage_at_the_Time_of_Interview.csv")

#splitting HT_race dataset by Indicator
HI_race <- Health_Insurance %>% 
	filter(Group == "By Race/Hispanic ethnicity") %>% 
	select(-Group, -State, -Phase, -'Time Period Label', -'Time Period Start Date', -'Time Period End Date')
HT_race_uninsured <- HI_race %>%
	filter(Indicator == "Uninsured at the Time of Interview") %>%
	select(-Indicator, -'Quartile Range')
HT_race_public <- HI_race %>%
	filter(Indicator == "Public Health Insurance Coverage")
HT_race_private <- HI_race %>%
	filter(Indicator == "Private Health Insurance Coverage")

#formatting HT_race_uninsured into wide format
HT_race_uninsured_L <- HT_race_uninsured %>%
	pivot_wider(id_cols = Subgroup, 
							names_from = "Time Period", 
							names_sep = "_", 
							values_from = c("Value", "Low CI", "High CI", "Confidence Interval")) %>%
	mutate(Value_1 = readr::parse_number(as.character(Value_1)))

#formatting HT_race_public into wide format
HT_race_public_L <- HT_race_public %>%
	pivot_wider(id_cols = Subgroup, 
							names_from = "Time Period", 
							names_sep = "_", 
							values_from = c("Value", "Low CI", "High CI", "Confidence Interval")) %>%
	mutate(Value_1 = readr::parse_number(as.character(Value_1)))

#formatting HT_race_private into wide format
HT_race_private_L <- HT_race_private %>%
	pivot_wider(id_cols = Subgroup, 
							names_from = "Time Period", 
							names_sep = "_", 
							values_from = c("Value", "Low CI", "High CI", "Confidence Interval")) %>%
	mutate(Value_1 = readr::parse_number(as.character(Value_1)))