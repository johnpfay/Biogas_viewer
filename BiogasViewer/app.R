#Shiny app to explore biogas potential


library(shiny)
library(leaflet)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points"),
  sliderInput("set_thresh",
              label = "Set Threshold",
              min=0, max=10, value=5
              )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Read in data
  dfPoultry <- read.csv('../data/raw/EWG_Poultry.csv')
  
  sliderValues <- reactive({
    dfSelect = dfPoultry %>% filter(Barns > input$set_thresh)
  })
  
  mapData <- eventReactive(input$recalc,{
    dfSelect = filter(dfPoultry, Barns > input$set_thresh)
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(data =  filter(dfPoultry, Barns < input$set_thresh)) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~X, lat = ~Y, radius= ~Barns,
                       stroke = FALSE, fillOpacity = 0.2
      )
  })
}

shinyApp(ui, server)

