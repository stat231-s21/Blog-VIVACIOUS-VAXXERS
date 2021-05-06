
library(readr)
library(tidyverse)
library(DT)
library(mosaic)
library(foreign)
AD <- read_csv("data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")
View(AD)

AD_race <- AD %>% 
  filter(Group == "By Race/Hispanic ethnicity") %>% 
  select(-Group, -State, -Phase, -'Time Period Label', -'Time Period Start Date', -'Time Period End Date')

#splitting AD_race dataset by Indicator  
AD_race_depression <- AD_race %>% 
  filter(Indicator == "Symptoms of Depressive Disorder") %>% 
  select(-Indicator, -'Quartile Range')
AD_race_anxiety <- AD_race %>% 
  filter(Indicator == "Symptoms of Anxiety Disorder")
AD_race_both <- AD_race %>% 
  filter(Indicator == "Symptoms of Anxiety Disorder or Depressive Disorder")

#formatting AD_race_depression into wide format
AD_race_depression_L <- AD_race_depression %>% 
  pivot_wider(
    id_cols = Subgroup, 
    names_from = "Time Period",
    names_sep = "_",
    values_from = c("Value", "Low CI", "High CI", "Confidence Interval")
  ) %>% 
  mutate(Value_1 = readr::parse_number(as.character(Value_1)))

#formatting AD_race_anxiety into wide format
AD_race_anxiety_L <- AD_race_anxiety %>% 
  pivot_wider(
    id_cols = Subgroup, 
    names_from = "Time Period",
    names_sep = "_",
    values_from = c("Value", "Low CI", "High CI", "Confidence Interval")
  ) %>% 
  mutate(Value_1 = readr::parse_number(as.character(Value_1)))

#formatting AD_race_both into wide format
AD_race_both_L <- AD_race_both %>% 
  pivot_wider(
    id_cols = Subgroup, 
    names_from = "Time Period",
    names_sep = "_",
    values_from = c("Value", "Low CI", "High CI", "Confidence Interval")
  ) %>% 
  mutate(Value_1 = readr::parse_number(as.character(Value_1)))

