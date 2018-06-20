# FIT5417 Assignment - Visualisation Project
# Author: Alexander Huang / 25961594
# Since: 23/05/2018
# Modified: 01/06/2018

# Install all required pakages
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("shinyjs")
# install.packages('rsconnect')
# install.packages("shinythemes")

# Load all required pakages
require(shiny)
require(ggplot2)
require(leaflet) # For map
require(plotly) # For plot
require(RColorBrewer) # For color of plot

# read the data into R
crashs <- read.csv("Crashes_Last_Five_Years.csv", comment.char="#")

# Divide data based on years
location_2012 = crashs[1:2027,]
location_2013 = crashs[2028:23028,]
location_2014 = crashs[23029:39395,]
location_2015 = crashs[39396:51768,]
location_2016 = crashs[51769:65765,]
location_2017 = crashs[65766:76449,]
speed2 = as.data.frame(table(crashs$SPEED_ZONE))
               
shinyServer(function(input, output){
  
  Sys.sleep(0.5)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  # Display the progress bar
  output$progressBar <- renderPlot({
    withProgress(message = 'Loading data...',
                 detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       
                       # Increment the progress bar, and update the detail text.
                       incProgress(0.1, detail = paste("part", i))
                       
                       # Pause for 0.1 seconds to simulate a long computation.
                       Sys.sleep(0.1)
                     }

                   }
                 )
    print("DONE")
  })
  
  # Display the bar chart for each speed zone 
  output$plotSpeedType <- renderPlotly({
    
    if (identical(input$speedType, "Year")){
      # Display the stacked bar chart for each speed zone in year
      plotSpeedYear()
    }else if (identical(input$speedType, "Amount of crashes for each speed zone")){
      # Display the stacked bar chart for each speed zone 
      plotSpeed()  
    }else if (identical(input$speedType, "Severity")){
      # Display the stacked bar chart for each speed zone in Severity
      plotSpeedSeverity()
    }else if (identical(input$speedType, "Urban")){
      # Display the stacked bar chart for each speed zone in urban
      plotSpeedUrban()
    }else{
      print ("end")
    }
  })
  
  # Display the pie chart for each road type
  output$plotRoadType <- renderPlotly({
    if (identical(input$road_type, "")){
      print ("Road Type loaded")
    }else if (identical(input$road_type, "Road type")){
      # Display the pie chart for each road type
      plotRoadType()
    }else if (identical(input$road_type, "Accident type")){
      # Display the pie chart for each accident type
      plotAccidentType()
    }else if (identical(input$road_type, "Road geometry")){
      # Display the pie chart for each road geometry
      plotRoad_geometry()
    }
    else{
      print ("ERROR: IN ROAD TYPE")
    }
  })
  
  # Display the map of crashes' location for selected year
  output$plotLocation <- renderLeaflet({
    if (identical(input$road_type, " ")){
      print ("Map Plot loaded")
    }else if (identical(input$selected_year, "2012")){
      plotMap(location_2012)
    }else if (identical(input$selected_year, "2013")){
      plotMap(location_2013)
    }else if (identical(input$selected_year, "2014")){
      plotMap(location_2014)
    }else if (identical(input$selected_year, "2015")){
      plotMap(location_2015)
    }else if (identical(input$selected_year, "2016")){
      plotMap(location_2016)
    }else if (identical(input$selected_year, "2017")){
      plotMap(location_2017)
    }
    else{
      print ("ERROR: IN MAP PLOT")
    }
  })
  
  output$about_var <- renderUI({
    #description <- paste("<b>",description,"</b>")
    description = "The purpose of this project is to develop an interactive data visualization application for drivers or anyone residing in the state of Victoria who wants to avoid the areas with high road accidents rates. The data involved in this project will be Victoria’s road crashes data for the last five years from VicRoads."
    description2 = "This application allows users to view the number of crashes occurred in the different speed zone, the exact locations of the previous road crashes and the accidents occurred in the different type of road. Please click the bar menu on left hand side to start."
      
    
    description <- paste("<h5>",description,"<br>","<br>",description2,"</h5>")
    #description <- div(description, style = "color:royalblue")
    HTML(paste(description))
  })
  
})

# ======================================================================================
# ======================== All the functions of ploting ================================
# ======================================================================================

# Plot the pie chart for each road geometry
plotRoad_geometry <- function(){
  roadGeometry = as.data.frame(table(crashs$ROAD_GEOMETRY))
  p4 <-
    plot_ly(labels = ~roadGeometry$Var1, values = ~roadGeometry$Freq,
            text = ~paste(roadGeometry$Var1,roadGeometry$Freq, sep='\n'), 
            hoverinfo = 'text',
            textinfo = "label+percent",
            marker = list(colors = brewer.pal(9, "Set3"),
                          line = list(color = '#FFFFFF', width = 0.1)),
            textposition = "inside") %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Number of crashes for each Road geometry",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           showlegend = TRUE
    )
  
  return(p4)
}

# Plot the pie chart for each accident type
plotAccidentType <- function(){
  
  # DCA(Definition for Classifying Accident) for different road type  
  accidentType = as.data.frame(table(crashs$DCA_CODE))
  
  p2 <- plot_ly(accidentType, labels = ~accidentType$Var1, values = ~accidentType$Freq, 
                textposition = "inside",
                textinfo = "percent",
                marker = list(colors = brewer.pal(12, "Paired"),
                              line = list(color = '#FFFFFF', width = 0.1)),
                text = ~paste(Var1,Freq, sep='\n'), 
                hoverinfo = 'text',
                showlegend = FALSE) %>%
    add_pie(hole = 0.6) %>%
    layout(title = 'Number of crashes for each Accident Type',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p2)
}

# Plot the pie chart for each road type
plotRoadType <- function(){
  
  # RMA(Road Management Act (2004) classification) for different road type  
  roadType = as.data.frame(table(crashs$RMA))
  # Remove road 
  roadType <- roadType[-c(1), ]
  p3 <- plot_ly(roadType,labels = ~roadType$Var1, values = ~roadType$Freq,
                text = ~paste(roadType$Var1,roadType$Freq, sep='\n'), 
                hoverinfo = 'text',
                textinfo = "label+percent",
                marker = list(colors = brewer.pal(9, "Accent"),
                              line = list(color = '#FFFFFF', width = 0.1)),
                textposition = "inside") %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Number of crashes for each type of road",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p3)
}

# Plot the stacked bar chart for each speed zone in urban
plotSpeedUrban<- function(){
  
  # ======================== To plot Bar chart for each urban ================================
  
  # This function is used for getting the amount of crashes each years for particular time zone
  speed_getUrban <- function(zone){
    # get all date records of crashes for this speed zone 
    UrabnData = crashs$DEG_URBAN_NAME[crashs$SPEED_ZONE == zone]
    # return a list content the amount for each years
    return(list("Country" = length(which(UrabnData=="Country")), "Large Provincial Cities" = length(which(UrabnData=="Large Provincial Cities")),
                "Metropolitan CBD Only" = length(which(UrabnData=="Metropolitan CBD Only")), "Metropolitan Excluding CBD" = length(which(UrabnData=="Metropolitan Excluding CBD")),
                "Other Towns" = length(which(UrabnData=="Other Towns")), "Small Provincial Cities" = length(which(UrabnData=="Small Provincial Cities")),
                "Small Towns" = length(which(UrabnData=="Small Towns"))
    ))
    
  }
  
  speed4 = as.data.frame(table(crashs$SPEED_ZONE))
  
  # Add crashes of each years for particular time zone into each speed zone
  for (zone in speed4$Var1 ){
    speed4$C[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Country"])
    speed4$LPC[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Large Provincial Cities"])
    speed4$MCO[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Metropolitan CBD Only"])
    speed4$MEC[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Metropolitan Excluding CBD"])
    speed4$OT[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Other Towns"])
    speed4$SPC[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Small Provincial Cities"])
    speed4$ST[speed4$Var1 == zone] <- as.numeric(speed_getUrban(zone)["Small Towns"])
  }
  
  p <- plot_ly(speed4, x = ~speed4$C, 
               y = ~reorder(speed4$Var1,speed4$Freq), 
               type = 'bar', orientation = 'h', name = 'Country',
               marker = list(color = '35b0ab',
                             line = list(color = 'rgba(58, 71, 80, 1.0)',
                                         width = 1.5))) %>%
    add_trace(x = ~speed4$LPC, name = 'Large Provincial Cities',
              marker = list(color = '446e5c',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed4$MCO, name = 'Metropolitan CBD Only',
              marker = list(color = '6a9c78',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed4$MEC, name = 'Metropolitan Excluding CBD',
              marker = list(color = '7dc383',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed4$OT, name = 'Other Towns',
              marker = list(color = 'c5f0a4',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed4$SPC, name = 'Small Provincial Cities',
              marker = list(color = 'fff1bc',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed4$ST, name = 'Small Towns',
              marker = list(color = 'faffb8',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           title = "Number of crashes for each speed zone (km/hr) in the type of urbanised area ",
           xaxis = list(title = "Amount of road crashes"),
           yaxis = list(title =""),
           margin = list(l = 150, r = 50, b = 50, t = 50, pad = 15,autoexpand = TRUE),
           font = list(family = "sans serif",size = 10,color = '446e5c'))
  
  # ======================================================================================
  return(p)
}

# Plot the stacked bar chart for each speed zone in Severity
plotSpeedSeverity <- function(){
  
  # ======================== To plot Bar chart for each severity ================================
  
  # This function is used for getting the amount of crashes each years for particular time zone
  speed_getSeverity <- function(zone){
    # get all date records of crashes for this speed zone 
    SeverityData = crashs$SEVERITY[crashs$SPEED_ZONE == zone]
    # return a list content the amount for each years
    return(list("Fatal accident" = length(which(SeverityData=="Fatal accident")), 
                "Non injury accident" = length(which(SeverityData=="Non injury accident")),
                "Serious injury accident" = length(which(SeverityData=="Serious injury accident")), 
                "Other injury accident" = length(which(SeverityData=="Other injury accident"))
    ))
  }
  
  speed3 = as.data.frame(table(crashs$SPEED_ZONE))
  
  # Add crashes of each years for particular time zone into each speed zone
  for (zone in speed3$Var1 ){
    speed3$FA[speed3$Var1 == zone] <- as.numeric(speed_getSeverity(zone)["Fatal accident"])
    speed3$NJA[speed3$Var1 == zone] <- as.numeric(speed_getSeverity(zone)["Non injury accident"])
    speed3$SIA[speed3$Var1 == zone] <- as.numeric(speed_getSeverity(zone)["Serious injury accident"])
    speed3$OIA[speed3$Var1 == zone] <- as.numeric(speed_getSeverity(zone)["Other injury accident"])
    
  }
  
  p <- plot_ly(speed3, x = ~speed3$FA, 
               y = ~reorder(speed3$Var1,speed3$Freq), 
               type = 'bar', orientation = 'h', name = 'Fatal accident',
               marker = list(color = '900d0d',
                             line = list(color = 'rgba(58, 71, 80, 1.0)',
                                         width = 1.5))) %>%
    add_trace(x = ~speed3$NJA, name = 'Non injury accident',
              marker = list(color = 'fffbcc',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed3$SIA, name = 'Serious injury accident',
              marker = list(color = 'cf1b1b',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed3$OIA, name = 'Other injury accident',
              marker = list(color = 'fd2e2e',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           title = "Number of crashes for each speed zone (km/hr) for every severity",
           xaxis = list(title = "Amount of road crashes"),
           yaxis = list(title =""),
           margin = list(l = 150, r = 50, b = 50, t = 50, pad = 15,autoexpand = TRUE),
           font = list(family = "sans serif",size = 10,color = 'c06c84'))
  
  # ======================================================================================
  
  return(p)
}

# Plot the stacked bar chart for each speed zone in year
plotSpeedYear <- function(){
  
  # ======================== To plot Bar chart for each year ================================
  
  # This function is used for getting the amount of crashes each years for particular time zone
  speed_getYear <- function(zone){
    # get all date records of crashes for this speed zone 
    yearData = crashs$ACCIDENT_DATE[crashs$SPEED_ZONE == zone]
    # convert this data to year
    yearData <- format(as.Date(yearData, format="%d/%m/%Y"),"%Y")
    # return a list content the amount for each years
    return(list("2012" = length(which(yearData=="2012")), "2013" = length(which(yearData=="2013")),
                "2014" = length(which(yearData=="2014")), "2015" = length(which(yearData=="2015")),
                "2016" = length(which(yearData=="2016")), "2017" = length(which(yearData=="2017"))
    ))
    
    # How to use: speed_getYear("75 km/hr")["2017"]
  }
  
  speed2 = as.data.frame(table(crashs$SPEED_ZONE))
  
  # Add crashes of each years for particular time zone into each speed zone
  for (zone in speed2$Var1 ){
    speed2$year_2017[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2017"])
    speed2$year_2016[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2016"])
    speed2$year_2015[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2015"])
    speed2$year_2014[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2014"])
    speed2$year_2013[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2013"])
    speed2$year_2012[speed2$Var1 == zone] <- as.numeric(speed_getYear(zone)["2012"])
  }
  
  p <- plot_ly(speed2, x = ~speed2$year_2017, 
               y = ~reorder(speed2$Var1,speed2$Freq), 
               type = 'bar', orientation = 'h', name = '2017',
               marker = list(color = '090089',
                             line = list(color = 'rgba(58, 71, 80, 1.0)',
                                         width = 1.5))) %>%
    add_trace(x = ~speed2$year_2016, name = '2016',
              marker = list(color = '0060ca',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed2$year_2015, name = '2015',
              marker = list(color = '91ceff',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed2$year_2014, name = '2014',
              marker = list(color = '58d5d3',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed2$year_2013, name = '2013',
              marker = list(color = '41a4c3',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    add_trace(x = ~speed2$year_2012, name = '2012',
              marker = list(color = '3f4b83',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           title = "Number of crashes for each speed zone (km/hr) for every year",
           xaxis = list(title = "Amount of road crashes"),
           yaxis = list(title =""),
           margin = list(l = 150, r = 50, b = 50, t = 50, pad = 15,autoexpand = TRUE),
           font = list(family = "sans serif",size = 10,color = '264e86'))
  
  # ======================================================================================
  
  return(p)
}

# Plot the bar chart for each speed zone 
plotSpeed <- function(){
  
  speed = as.data.frame(table(crashs$SPEED_ZONE))
  speedP <- 
    plot_ly(
      x = ~speed$Freq,
      y = ~reorder(speed$Var1,speed$Freq),
      text = ~paste("Crahses in this speed: ",speed$Freq, sep='\n'),
      hoverinfo = 'text',
      type = "bar",
      marker = list(color = '3f4b83')) %>%
    layout(title = "Number of crashes for each speed zone (km/hr)",
           xaxis = list(title = "Number of Crash"), 
           yaxis = list(title = " ",ticklen=5),
           margin = list(l = 150, r = 50, b = 50, t = 50, pad = 15,autoexpand = TRUE),
           font = list(family = "sans serif",size = 10,color = '264e86'))
  
  return(speedP)
}

# Plot the map of crashes' location for selected year
plotMap <- function(year){
  
  myColor <- colorFactor(palette = c("#ff4d4d", "white","#ffe981","#ff8364"), 
                         domain = c("Fatal accident","Non injury accident","Serious injury accident" ,"Other injury accident"),
                         ordered=FALSE)
  
  # Prepar the text for the tooltip:
  mytext=paste("Date: ", year$ACCIDENT_DATE, "<br/>", "Local Government: ", year$LGA_NAME, "<br/>", 
               "Severity: ", year$SEVERITY,"<br/>","Persons involved: ", year$TOTAL_PERSONS, 
               sep="") %>%
    lapply(htmltools::HTML)
  
  # Final Map
  myMap <- leaflet(year) %>% 
    addTiles()  %>% 
    setView( lat=-37.4713, lng=144.7852 , zoom=6) %>%
    # Adding map type for user to select
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addProviderTiles("OpenMapSurfer.Roads", group="Default") %>%
    addProviderTiles("Esri.DeLorme", group="Road map") %>%
    addProviderTiles("Esri.WorldStreetMap", group="Street") %>%
    addLayersControl(baseGroups = c("Default","Satellite","Roadmap","Street"), 
                     options = layersControlOptions(collapsed = FALSE))%>%
    
    addCircleMarkers(~year$LONGITUDE, ~year$LATITUDE, 
                     fillColor = ~myColor(year$SEVERITY), fillOpacity = 0.8, 
                     color= ~myColor(year$SEVERITY), radius=4, stroke=FALSE,
                     label = mytext,
                     labelOptions = labelOptions( style = 
                                                    list("font-weight" = "normal", padding = "3px 8px"), 
                                                  textsize = "13px", direction = "auto"))%>%
    addLegend(colors=c("#ff4d4d", "white","#ffe981","#ff8364"), 
              labels=c("Fatal accident","Non injury accident","Serious injury accident" ,"Other injury accident"), 
              opacity=0.9,title = "Severity", position = "bottomright" )
  
  
  
  
  return(myMap)
}
