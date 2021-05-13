library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(maps)
library(viridis)

##########################################################################
############################## import data ###############################
##########################################################################
mental <- read_csv("Ayo ShinyApp/map_mental_health.csv")
health <- read_csv("Ayo ShinyApp/map_health.csv")
provider <- read_csv("Ayo ShinyApp/map_provider.csv")
covid <- read_csv("Ayo ShinyApp/map_covid.csv")
##########################################################################
######## define choice values and labels for widgets (user inputs) #######
##########################################################################
# for TAB 1 (Spatial) widgets: 
# for selectInput, 'choices' object should be a NAMED LIST
race_choice_values <- c("all_adults", "black", "white", "asian")
race_choice_names <- c("All Adults", "Black", "White", "Asian")
names(race_choice_values) <- race_choice_names
#for selectInput state
state_choices <- unique(usa_mental_health$state)

##########################################################################
################################    ui    ################################
##########################################################################
ui <- navbarPage(
  
  title="Racial Health Inequities",
  
  tabPanel(
    title = "Mental Health",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "racevar"
                    , label = "Choose a racial group:"
                    , choices = race_choice_values
                    , selected = "all_adults")
      ),
      mainPanel(
        plotOutput(outputId = "mental")
      )
    )
  ),
  tabPanel(
    title = "Overall Health",
    
    sidebarLayout(
      
      sidebarPanel(
        selectizeInput(inputId = "racevar1"
                       , label = "Choose a racial group:"
                       , choices = race_choice_values
                       , selected = "all_adults")
      ),
      mainPanel(
        plotOutput(outputId = "health")
      )
    )
  ),
  tabPanel(
    title = "Provider Access",
    
    sidebarLayout(
      
      sidebarPanel(
        selectizeInput(inputId = "racevar2"
                       , label = "Choose a racial group:"
                       , choices = race_choice_values
                       , selected = "all_adults")
      ),
      mainPanel(
        plotOutput(outputId = "provider")
      )
    )
  ),
  tabPanel(
    title = "COVID",
    
    sidebarLayout(
      
      sidebarPanel(
        selectizeInput(inputId = "racevar3"
                       , label = "Choose a racial group:"
                       , choices = race_choice_values
                       , selected = "all_adults")
      ),
      mainPanel(
        plotOutput(outputId = "covid")
      )
    )
  )
)
##########################################################################
################################  server  ################################
##########################################################################
server <- function(input,output){
  
  # TAB Mental Health #: 

  output$mental <- renderPlot({
    ggplot(data = mental
           , aes_string(x = "long"
                       , y = "lat"
                       , group = "group"
                       , fill = input$racevar)) +
      geom_polygon(colour = "grey") +
      scale_fill_viridis(option = "D", direction = 1) +
      theme_void() +
      coord_fixed(ratio = 1.3)+
      labs(x = race_choice_names[race_choice_values == input$racevar],
           fill = "Average Number of Poor Mental Health Days Reported \n in the last 30 days",
           caption = "* States with 0 days reported may have missing data",
           title = "Looking at Mental Health by Racial Group",
           subtitle= "in the United States in 2019")
      

                     
  })
  # TAB Overall health #: 
  
  output$health <- renderPlot({
    ggplot(data = health
           , aes_string(x = "long"
                        , y = "lat"
                        , group = "group"
                        , fill = input$racevar1)) +
      geom_polygon(colour = "grey") +
      scale_fill_viridis(option = "D", direction = 1) +
      theme_void() +
      coord_fixed(ratio = 1.3)+
      labs(x = race_choice_names[race_choice_values == input$racevar1],
           fill = "Number of Fair or Poor Health Days",
           caption = "* States with 0 days reported may have missing data",
           title = "Adults Who Report Fair/Poor Mental Health Days",
           subtitle= "in the United States in 2019")
    
    
    
  })
  # TAB provider #: 
  
  output$provider <- renderPlot({
    ggplot(data = provider
           , aes_string(x = "long"
                        , y = "lat"
                        , group = "group"
                        , fill = input$racevar2)) +
      geom_polygon(colour = "grey") +
      scale_fill_viridis(option = "D", direction = 1) +
      theme_void() +
      coord_fixed(ratio = 1.3)+
      labs(x = race_choice_names[race_choice_values == input$racevar2],
           fill = "Percentage of Adults",
           caption = "* States with 0 adults reported may have missing data",
           title = "Adults Who Report Not Seeing a Doctor n\ in the Past 12 Months Because of Cost",
           subtitle= "In the United States in 2019")  
 
}) 
  # TAB COVID #: 
  
  output$covid <- renderPlot({
    ggplot(data = covid
           , aes_string(x = "long"
                        , y = "lat"
                        , group = "group"
                        , fill = input$racevar3)) +
      geom_polygon(colour = "grey") +
      scale_fill_viridis(option = "D", direction = 1) +
      theme_void() +
      coord_fixed(ratio = 1.3)+
      labs(x = race_choice_names[race_choice_values == input$racevar3],
           fill = "Number of COVID Deaths",
           caption = "* States with 0 days reported may have missing data",
           title = "Number of COVID Deaths",
           subtitle= "in the United States as of May 2021")  
    
  })

}

##########################################################################
############################ call to shinyApp ############################
##########################################################################
shinyApp(ui = ui, server = server)
