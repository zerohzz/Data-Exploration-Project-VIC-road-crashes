# FIT5417 Assignment - Visualisation Project
# Author: Alexander Huang / 25961594
# Since: 23/05/2018
# Modified: 01/06/2018

# Load all required pakages
require(shiny)
require(ggplot2)
require(leaflet) # For map
require(plotly) # For plot
require(RColorBrewer) # For color of plot
require(shinyjs) # For loading page 
require(shinythemes) # For loading the themes for the shiny app


appCSS <- "
#loading-content {
  position: absolute;
  background: #3d6cb9;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #fafaf6;
}
"

shinyUI(
  fluidPage(
    theme = shinytheme("yeti"),
    useShinyjs(),
    inlineCSS(appCSS),
    
    # Loading message
    div(
      id = "loading-content",
      h3("This is the Interactive visualisation of Victoria's road crashes data."),
      
      h1("The application is loading..."),
      
      h4("FIT5147 - Data exploration and visualisation Assignment: Visualisation Project developed by Alexander Huang", 
            style = "
            text-align:center;
            position:absolute;
            bottom:0;
            ")
      
    ),
    
    # The main app code goes here
    hidden(
      div(
        id = "app-content",
        titlePanel("Interactive visualisation of Victoria's road crashes data in last five years(2012 - 2017)."),

        # h1("Interactive visualisation of Victoria's road crashes data in last five years(2012 - 2017).",style = "color:royalblue")
    
      
        navlistPanel(
          "You can see the: ",
          
          # First tab: description of this application
          tabPanel("About this application",
                   # adding the new div tag to the sidebar            
                   tags$div(class="header", checked=NA,
                            tags$p(h4("This application is part of the FIT5147 Visualisation Project Assignment. ",style = "color:royalblue"))
                            
                   ),
                   htmlOutput("about_var"),
                   plotOutput("progressBar")
                   
          ),
          
          # Second tab: showing bar chart of crashes for each speed zone  
          tabPanel("Speed of accidents",
                   
                   tags$div(
                     
                     h4("In this page, you can see the data of crashes for each speed zone."),
                     radioButtons("speedType", "You can choose classifications and move your mouse to see more detail:", 
                                  choices = c("Amount of crashes for each speed zone", 
                                              "Year","Severity","Urban"),
                     ),
                     
                     plotlyOutput("plotSpeedType",height = "600px"),
                     
                     h5("Notes: The dataset is huge so it may take few minutes to load the graphs. If the graph is not displayed in 3 minutes, please click another button and try again.",
                        style = "color:royalblue")
                     
                   )), 
          
          # Third tab: showing map for location of crashes 
          tabPanel("Location of accidents", 
                   h4("In this page, you can see the location  of crashes."),
                   
                   helpText("Please select the year to view the location of crashes."),
                   
                   selectInput("selected_year", "Select the year:",
                               list(
                                 #"Please select a year" = "",
                                 "2017" = "2017", 
                                 "2016" = "2016", 
                                 "2015" = "2015",
                                 "2014" = "2014",
                                 "2013" = "2013",
                                 "2012" = "2012"),
                               selected = "Please select"),
                   
                   leafletOutput("plotLocation"),
                   h5("Notes: The dataset is huge so it may take few minutes to load the graphs. If the graph is not displayed in 3 minutes, please click another button and try again.",
                      style = "color:royalblue")
                   
          ), 
          
          # Fourth tab: showing pie chart for crashes' type of roads
          tabPanel("Types of road",
                   
                   h4("In this page, you can see the data of crashes for type of roads."),
                   
                   radioButtons("road_type", "Please select the classification: ", 
                                choices = c("Road geometry", 
                                            "Accident type",
                                            "Road type")),
                   
                   plotlyOutput("plotRoadType",height = "600px"),
                   h5("Notes: The dataset is huge so it may take few minutes to load the graphs. If the graph is not displayed in 3 minutes, please click another button and try again.",
                      style = "color:royalblue")
                   
          )
        )
        
      )
    )
  )
)
