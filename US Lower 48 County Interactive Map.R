#I learned to use Leaflet and Shiny from this guied http://boazsobrado.com/blog/2018/02/08/leaflet-timeline-in-r/


# this is a shiny web app. Save as app.r

library(shiny)
library(lubridate)
library(leaflet)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(directlabels)
library(rjson)
library(jsonlite)
library(urltools)

## Define UI for application that draws a map
data<- 
nytimes_url<-URLencode("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
statecov <- read_csv(nytimes_url)
  ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mapAct", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("selection", "Date:",
                              min = as.Date("2020-01-21"),
                              max = max(as.Date(statecov$date)),
                              value = as.Date("2020-03-10"))
    )
    
  )

tag.map.title <- tags$style(HTML("
                                 .leaflet-control.map-title { 
                                 transform: translate(-50%,20%);
                                 position: fixed !important;
                                 left: 30%;
                                 text-align: left;
                                 padding-left: 5px; 
                                 padding-right: 5px; 
                                 background: rgba(255,255,255,0.75);
                                 font-weight: bold;
                                 font-size: 12px;
                                 }
                                 "))

title <- tags$div(
  tag.map.title, HTML("COVID-19 Confirmed Cases")
)  

# Define server logic required
server <- function(input, output) {
  #grab data first time
  #Covid County Data

  nytimes_url<-URLencode("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  statecov <- read_csv(nytimes_url)
  
  
  #Removing unknown county
  statecov<-statecov[which(statecov$county!="Unknown"),]
  
  #read in Fips Codes and Lat Lon
  #https://www.weather.gov/gis/Counties
  #sf<-st_read("./c_02jn20.dbf")
  #fips_lat_long<-sf[,c("FIPS","LON","LAT")]
  #remove the polygons
  #fips_lat_long_nogeo <- st_set_geometry(fips_lat_long, NULL) 
  #names(fips_lat_long_nogeo)[1]<-"fips"
  load("./fips_lat_long_nogeo.RData")
  fips_lat_long_nogeo2<-fips_lat_long_nogeo[!duplicated(fips_lat_long_nogeo$fips), ]
  
  
  statecov<-statecov[order(statecov$fips),]
  #Fix NYC and KS City Misouri
  #NYC 36061
  #KS City 29095
  statecov$fips[statecov$county=="Kansas City" & statecov$state=="Missouri"]<-29095
  statecov$fips[statecov$county=="New York City" & statecov$state=="New York"]<-36061
  statedata<-merge(x = statecov, y =fips_lat_long_nogeo2, by = "fips", all.x = TRUE)
  names(statedata)[7:8]<-c("longitude","latitude")

  statedata<-statedata[order(statedata$state,statedata$county,statedata$date),]
  statedata$TextLabel<-paste(statedata$state,statedata$county,statedata$cases)
  
  
  #stuff in server
  filteredData <- reactive({
    statedata %>% filter(date == input$selection)
  })
  #https://stackoverflow.com/questions/31406598/leaflet-in-r-setview-based-on-range-of-latitude-and-longitude-from-dataset
  output$mapAct<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = -130,lat1 = 25,lng2 = -60,lat2 = 50)
  })
  
  observeEvent(input$selection,{
    leafletProxy("mapAct", data = filteredData()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addCircleMarkers(radius =~ 2.2*log(cases),
                       lng = ~longitude,
                       lat = ~latitude,
                       stroke=FALSE,
                       fillOpacity = 0.35,
                       color = "red",
                       label =~TextLabel,
                       labelOptions = labelOptions(noHide = FALSE, offset=c(0,-5), 
                                                   direction='top', textsize='20px', textOnly = TRUE)) %>%
      addControl(title, position = "topleft", className="map-title") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)