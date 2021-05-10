library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(shinyWidgets)

##########################################################################
############################## import data ###############################
##########################################################################
AD <- read_csv("data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv") %>%
	filter(Group == "By Race/Hispanic ethnicity") %>% 
	select(-Group, -State, -Phase, -'Time Period Label', -'Time Period Start Date', 
				 -'Time Period End Date', -'Confidence Interval') %>% 
	filter(Indicator == "Symptoms of Depressive Disorder") %>% 
	select(-Indicator, -'Quartile Range')


##########################################################################
######## define choice values and labels for widgets (user inputs) #######
##########################################################################

# for selectInput Race
Subgroup_choices <- unique(AD$Subgroup)

# for selectInput Values
value_values <- c("Value_2", "Value_3", "Value_4", "Value_5", "Value_6", "Value_7", 
						"Value_8", "Value_9", "Value_10", "Value_11", "Value_12", "Value_13", "Value_14",
						"Value_15", "Value_16", "Value_17", "Value_18", "Value_19", "Value_20", "Value_21",
						"Value_22", "Value_23", "Value_24", "Value_25", "Value_26", "Value_27", "Value_28")
value_names <- c("Value_2", "Value_3", "Value_4", "Value_5", "Value_6", "Value_7", 
					 "Value_8", "Value_9", "Value_10", "Value_11", "Value_12", "Value_13", "Value_14",
					 "Value_15", "Value_16", "Value_17", "Value_18", "Value_19", "Value_20", "Value_21",
					 "Value_22", "Value_23", "Value_24", "Value_25", "Value_26", "Value_27", "Value_28")
names(value_values) <- value_names

##########################################################################
################################    ui    ################################
##########################################################################
ui <- fluidPage(
	title = "Depression by Race",
	
	sidebarPanel(
		title = "Bar Graph",
		sidebarLayout(
			sidebarPanel(
				selectInput("Subgroup"
											 , label = "Please choose a race to observe: "
											 , choices = Subgroup_choices
											 , selected = 'Hispanic or Latino',
											 multiple = TRUE),
				
				selectInput(inputId = "value"
										, label = "Choose a value time period of interest:"
										, choices = value_values
										, selected = "Value_2"),
			),
			mainPanel(plotOutput("bar", height = 500))
		)
	)
)

##########################################################################
################################  server  ################################
##########################################################################
server <- function(input, output){
	
	AD_reactive <- reactive({
		data <- AD %>%
			filter(value_type == input$value,
						 Subgroup %in% input$Subgroup)
	})
	
	output$bar <- renderPlot({
		#req(input$id_name)
		ggplot(data = AD_reactive, aes_string(x = "Subgroup", y = "value",
																									color = "Subgroup")) +
			geom_bar(stat = "identity", width = 0.8) + labs(x = "Race", y = "Value of Depression")
	})
}

##########################################################################
############################ call to shinyApp ############################
##########################################################################
shinyApp(ui = ui, server = server)
