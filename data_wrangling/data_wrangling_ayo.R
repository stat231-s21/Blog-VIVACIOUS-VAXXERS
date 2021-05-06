library(tidyverse)
library(DT)
library(mosaic)
library(foreign)
suicide <- read_csv("data/suicide-lghc-indicator-21.csv") %>% 
	janitor::clean_names()

california <- subset(suicide, geography == "CALIFORNIA")
counties <- subset(suicide, geography != "CALIFORNIA")

california_race <-subset(california, strata == "Race/Ethnicity") %>%
	pivot_wider(id_col= year, names_from = strata_name, 
							names_sep = "_", values_from = rate) %>%
	janitor::clean_names() %>%
	select(year, asian_nh, black_nh)
write_csv(california_race, file = "data/cal_race.csv")

counties_race <-subset(counties, strata == "Race/Ethnicity") %>%
	pivot_wider(id_col= c(year, geography), names_from = strata_name, 
							names_sep = "_", values_from = rate) %>%
	janitor::clean_names() %>%
	select(year, geography, asian_nh, black_nh)
write_csv(counties_race, file = "data/cal_county_race.csv")

depression <- read_csv("data/adult-depression-lghc-indicator-2.csv") %>% 
	janitor::clean_names() %>%
	subset(strata == "Race-Ethnicity") 
depression_race <- depression %>%
	pivot_wider(id_col= year, names_from = strata_name, 
							names_sep = "_", values_from = percent)%>%
	janitor::clean_names() %>%
	select(year, asian_pacific_islander, black)
write_csv(depression_race, file = "data/depression_race.csv")
