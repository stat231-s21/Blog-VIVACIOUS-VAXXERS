AD <- read_csv("data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv") %>%
	filter(Group == "By Race/Hispanic ethnicity") %>% 
	select(-Group, -State, -Phase, -'Time Period Label', -'Time Period Start Date', 
				 -'Time Period End Date', -'Confidence Interval') %>% 
	filter(Indicator == "Symptoms of Depressive Disorder") %>% 
	select(-Indicator, -'Quartile Range') %>%
	filter(!is.na(Value))
write.csv(AD, file = "AD.csv")


