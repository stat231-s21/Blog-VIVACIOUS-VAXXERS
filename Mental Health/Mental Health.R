library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(shinyWidgets)
library(ggplot2)

##########################################################################
############################## import data ###############################
##########################################################################
AD <- read_csv("~/Spring 2021/Data Science - STAT 231/Blog-VIVACIOUS-VAXXERS/Mental Health/AD.csv") %>%
	rename("Time_Period" = "Time Period") %>%
	mutate("Time_Character" = as.character(Time_Period))


##########################################################################
######## define choice values and labels for widgets (user inputs) #######
##########################################################################
# for TAB 1 Time Series Line Graph widget:
subgroup_choices <- unique(AD$Subgroup)

# for TAB 2 Bar Graph widget:
timeperiod_choices <- unique(AD$Time_Period)



##########################################################################
################################    ui    ################################
##########################################################################
ui <- navbarPage(
	title = "COVID-19's Impact on Anxiety and Depression by Race",
	
	tabPanel(
		title = "Time Series Line Graph",
		sidebarLayout(
			sidebarPanel(
					selectizeInput(inputId = "Race"
										, label = "Choose a subgroup of interest:"
										, choices = subgroup_choices
										, selected = "Hispanic or Latino"),
			),
			mainPanel(plotOutput(outputId = "line"))
		)
	),
	
	tabPanel(
		title = "Bar Chart",
		sidebarLayout(
			sidebarPanel(
				selectInput(inputId = "Time_Character"
										, label = "Choose a time period to visualize:"
										, choices = timeperiod_choices
										, selected = "1"),
			),
			mainPanel(plotOutput(outputId = "bar"))
		)
	)
)

##########################################################################
################################  server  ################################
##########################################################################
server <- function(input, output){
	# Tab 1: Time Series Line Graph
	AD_reactive <- reactive({
		data <- AD %>%
			filter(Subgroup %in% input$Race)
	})
	Color_reactive <- reactive({
		if(input$Race == "Hispanic or Latino"){
			color <- "#FF5733"
		}
		if(input$Race == "Non-Hispanic White, single race"){
			color <- "#33FF57"
		}
		if(input$Race == "Non-Hispanic Black, single race"){
			color <- "#FFBD33"
		}
		if(input$Race == "Non-Hispanic Asian, single race"){
			color <- "#ff637a"
		}
		if(input$Race == "Non-Hispanic, other races and multiple races"){
			color <- "#EE33FF"
		}
		return(color)
		
	})
	
	output$line <- renderPlot({
		ggplot(data = AD_reactive(), aes(x = Time_Period, y = Value)) +
			geom_line(color = Color_reactive()) + geom_point(color = Color_reactive()) + labs(x = "Time Period", y = "Value of Depression/Anxiety")
	})
	
	
	# Tab 2: Bar Chart
	AD_reactive_b <- reactive({
		data_b <- AD %>%
			filter(Time_Character %in% input$Time_Character)
	})
	
	output$bar <- renderPlot({
		ggplot(data = AD_reactive_b(), aes(x = Subgroup)) +
			geom_bar(color = Subgroup, stat = "identity") + labs(x = "Subgroup", y = "Value of Depression/Anxiety")
	})
}

##########################################################################
############################ call to shinyApp ############################
##########################################################################
shinyApp(ui = ui, server = server)
