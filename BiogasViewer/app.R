#Shiny app to explore biogas potential
library(shiny)
library(leaflet)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  sliderInput("set_thresh",
              label = "Set Threshold",
              min=0, max=10000, value=500
              )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Read in data
  dfPoultry <- read.csv('../data/raw/EWG_Poultry.csv')
  updateSliderInput(session,'set_thresh',max=max(dfPoultry$BIRD_COUNT))
  

  output$mymap <- renderLeaflet({
    leaflet(data =  filter(dfPoultry, BIRD_COUNT > input$set_thresh)) %>%
      setView(lng=-79.8373764,lat=35.5465094,zoom=7) %>% 
      addTiles() %>%
      addCircleMarkers(lng = ~X, lat = ~Y, radius= ~Barns,
                       stroke = FALSE, fillOpacity = 0.2
      )
  })
}

shinyApp(ui, server)

