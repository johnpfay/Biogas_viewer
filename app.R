#Shiny app to explore biogas potential
library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  selectInput(inputId = "dataset",
              label="Select biogas source:",
              choices=c('Poultry','Swine')),
  p(),
  sliderInput("set_thresh",
              label = "Minimum Biogas Potential (MMBTU_potential)",
              min=0, max=10000, value=500
              )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Read in data
  dfPoultry <- read.csv('./data/raw/EWG_Poultry.csv')
  dfSwine <- read.csv('./data/raw/EWG_Swine_etan2.csv')
  dfSwine$MMBTU_potential <- as.numeric(as.character(dfSwine$MMBTU_potential))
  dfCattle <- read.csv('./data/raw/EWG_Cattle.csv')
  fcPipes <- readOGR(dsn='./data/spatial/Natural_Gas_Liquid_Pipelines_NC.shp')
  
  #Dataset input
  datasetInput <- reactive({
    switch(input$dataset,
           'Poultry' = dfPoultry,
           'Swine' = dfSwine,
           'Cattle' = dfCattle)
    
  })
  
  #Slider for MMBTUS
  updateSliderInput(session,'set_thresh',
                    max=max(dfPoultry$MMBTU_potential),
                    value=max(dfPoultry$MMBTU_potential)/2
                    )
  

  output$mymap <- renderLeaflet({
    dfData = datasetInput()
    leaflet(data =  filter(dfData, MMBTU_potential > input$set_thresh)) %>%
      setView(lng=-79.8373764,lat=35.5465094,zoom=7) %>% 
      addPolylines(data=fcPipes,color='black',opacity=0.5,weight=2) %>% 
      addTiles() %>%
      addCircleMarkers(lng = ~X, lat = ~Y, radius= ~MMBTU_potential/1000,
                       stroke = FALSE, fillOpacity = 0.2
      )
  })
}

shinyApp(ui, server)

