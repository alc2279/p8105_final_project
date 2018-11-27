


library(shiny)

library(leaflet)
library(RColorBrewer)
library(DT)
library(rgdal)
library(gpclib)
library(maptools)
library(R6)
library(raster)
library(broom)
library(scales)
library(reshape2)
library(tidyverse)
library(data.table)
library(highcharter)

#Read in dataset
Data = fread('https://chronicdata.cdc.gov/api/views/6vp6-wxuq/rows.csv?accessType=DOWNLOAD')


#Specify shinyApp(ui, server)
shinyApp(options = list(height = 800), 
         
         #Define the user interface element
         ui = fluidPage(
           fluidRow(
             column(5
                    
                    #Create element to allow user input. 
                    , selectInput('categoryId', 'Select Category'
                                  , choices = unique(Data$CategoryID)
                    )
                    #render ui elements within the server function. 
                    , uiOutput('measures'))
             
             , column(3, uiOutput('slider')
                      , selectInput('age', 'Type', choices = unique(Data$DataValueTypeID))
             )
           )
           ,fluidRow(column(8, leafletOutput('mymap'))
                     , column(4, dataTableOutput('table'))
           )
         )
         
         #Define functionality
         ,server = function(input, output, session){
           
           
           #Read the data into a reactive function. 
           df1 = reactive ({
             df = Data
             df = subset(df, select = c('CityName','StateAbbr', 'GeoLocation', 'Year'
                                         , 'Measure', 'Data_Value', 'PopulationCount', 'GeographicLevel'
                                         , 'Short_Question_Text', 'CategoryID', 'DataValueTypeID'))
             
             #Removes NA values
             df = df[!is.na(df$Data_Value),]
             df = subset(df, DataValueTypeID == input$age)
             df = subset(df, CategoryID == input$categoryId)
             #df = subset(df, GeographicLevel == as.character(input$geoLevel))
           })
           
           #renderUI to dynamically generate ui elements. 
           output$measures = renderUI({
             selectInput('measures', 'Select Measure', choices = unique(df1()$Measure))
           })
           
           #Filter the data again based on the measure chosen by the user. 
           df2 = reactive ({
             x = df1()
             x = subset(x, Measure == as.character(input$measures))
           })
           
           #Create a slider to filter the map markers. 
           output$slider = renderUI ({
             sliderInput('slider', 'Filter Map', min = min(df2()$Data_Value) 
                         , max = max(df2()$Data_Value)
                         , value = c(min(df2()$Data_Value), max(df2()$Data_Value)))
           })
           
           #Build the leaflet map
           output$mymap = renderLeaflet({
             df = df2()
             
             #Filter the data set based on values from the slider input
             df = subset(df, Data_Value > input$slider[1] & Data_Value < input$slider[2])
             
             #Define color pallete
             Colors = brewer.pal(8,"Set2")
             
             #Apply pallete to values from data set
             binpal = colorBin(Colors, df$Data_Value, 6, pretty = FALSE)
             
             #Separate GeoLocation column into latitude and longitude columns. 
             lat = vector()
             lng = vector()
             for (i in 1: nrow(df)){
               x= unlist(strsplit(df$GeoLocation[i], ",")) 
               lat[i] = substr(x[1],2,8) 
               lng[i] = substr(x[2],2,9) 
               
             }
             
             df$lat = as.numeric(lat)
             df$lng = as.numeric(lng)
             
             #Build leaflet map
             leaflet() %>%
               
               #Adds state borders to the map
               addTiles(
                 urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
               ) %>%
               
               #Add the markers for each location
               addCircleMarkers(lat = df$lat
                                , lng = df$lng
                                , data = df
                                , label = paste(df$CityName, df$StateAbbr)
                                , color = ~binpal(Data_Value)
                                , radius = 5
                                , fillColor = ~binpal(Data_Value)
                                , fill = TRUE
                                , opacity = 0.8
                                
               ) %>%
               addLegend(position = 'bottomleft', pal = binpal, values = df$Data_Value
               )
             
           })
           
           #Create data table to show values in tabular format
           output$table = renderDataTable ({
             df = subset(df2(), select = c(CityName, StateAbbr, Data_Value))
             df = df[order(df$Data_Value, decreasing = TRUE),]
             df = setNames(df, c('City', 'State', 'Value'))
             datatable(df, options = list(pageLength = 10))
             
           })
           
         }
)

