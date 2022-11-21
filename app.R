#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)


ui <- dashboardPage(
  dashboardHeader(title = "Shiny",titleWidth=700),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map1",tabName ="map1",icon=icon("map-location-dot","fa-solid fa-map-location-dot")),
      menuItem("Table",tabName ="table",icon=icon("table","fa-solid fa-table")),
      menuItem("Map2",tabName ="map2",icon=icon("map","fa-solid fa-map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map1",
              fluidRow(
                leafletOutput("map1")
              )
      )
    ),
    tabItems(
      tabItem(tabName = "table",
              fluidRow(
                dataTableOutput("table")
              )
      )
    ),
    tabItems(
      tabItem(tabName = "map2",
              fluidRow(
                plotOutput("map2",brush = "shape_brush")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  bike <- read.csv(url("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::blue-bike-stations.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
  new <- bike %>%
    group_by(District) %>% 
    summarise(total_count=n()) %>%
    select(District, total_count)
  
  ggplot(data=new, aes(x=District, y=total_count)) +
    geom_bar(stat="identity")
  output$map2<- renderPlot(ggplot(data = new, aes(x=District, y=total_count)) + 
                             geom_bar(stat="identity"))
  
  
  output$table <- renderTable({
    brushedPoints(bike, input$shape_brush)
  })
  
  tab <-  bike %>% 
    select("Number","Name","Total_docks")
  output$table <- renderDataTable(tab, options = list(pageLength = 5))
  
  output$map1<-renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
      addMarkers(lng = bike$Longitude, 
                 lat = bike$Latitude, 
                 popup = bike$Number)
  })
}
shinyApp(ui, server)

