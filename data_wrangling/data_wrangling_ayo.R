#wrangling
library(tidyverse)
library(DT)
library(mosaic)
library(foreign)
#mapping
library(datasets)
library(mdsr)
library(gapminder)
library(leaflet)
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
#mental health
mentalHealth <- read_csv("data/mental_health.csv") %>%
	janitor::clean_names() %>%
	mutate(state = tolower(location)) %>%
	select(state, all_adults, black, white, asian_native_hawaiian_or_pacific_islander)
usa_states <- map_data(map = "state"
											 , region = ".") 
usa_mental_health <- mentalHealth %>%
	inner_join(usa_states, by = c("state"="region")) %>%
	rename(asian = asian_native_hawaiian_or_pacific_islander) %>%
	mutate(asian = ifelse(asian == "NSD", 0 , asian)) 
write_csv(usa_mental_health, file = "data/map_mental_health.csv")
#provider
provider <- read_csv("data/Provider_Access.csv") %>%
	janitor::clean_names() %>%
	mutate(state = tolower(location)) %>%
	rename(asian = asian_native_hawaiian_or_pacific_islander) %>%
	select(state, all_adults, black, white, asian)
map_provider <- provider %>%
	inner_join(usa_states, by = c("state"="region")) %>%
	mutate(asian = ifelse(asian == "NSD", 0 , asian)) 
write_csv(map_provider, file = "data/map_provider.csv")
#overall health
health <- read_csv("data/overall_health.csv") %>%
	janitor::clean_names() %>%
	mutate(state = tolower(location)) %>%
	rename(asian = asian_native_hawaiian_or_pacific_islander) %>%
	select(state, all_adults, black, white, asian)
map_health <- health %>%
	inner_join(usa_states, by = c("state"="region")) %>%
	mutate(asian = ifelse(asian == "NSD", 0 , asian)) 
write_csv(map_health, file = "data/map_health.csv")
#covid deaths
covid <- read_csv("data/COVID_deaths.csv") %>%
	janitor::clean_names() %>%
	mutate(state = tolower(location)) %>%
	rename(asian = asian_native_hawaiian_or_pacific_islander) %>%
	select(state, all_adults, black, white, asian)
map_covid <- health %>%
	inner_join(usa_states, by = c("state"="region")) %>%
	mutate(asian = ifelse(asian == "NSD", 0 , asian)) 
write_csv(map_covid, file = "data/map_covid.csv")