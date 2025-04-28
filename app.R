library(shiny)
library(leaflet)
library(tidyverse)
library(janitor)
library(tidygeocoder)
library(scales)

mn_sample_geo <- read_csv("Data/mn_sf.csv")

mn_sample_geo <- mn_sample_geo %>%
  mutate(popup_info = paste0(
    "<strong>", name, "</strong><br>",
    street, "<br>",
    city, ", ", state, " ", zip, "<br>",
    "Revenue: $", comma(revenue_amt, accuracy = 1)
  ))

ui <- fluidPage(
  titlePanel("Minnesota Nonprofits Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("revenue_range", "Minimum Revenue:",
                  min = 0, 
                  max = max(mn_sample_geo$revenue_amt, na.rm = TRUE),
                  value = 0, step = 10000),
      
      sliderInput("asset_range", "Minimum Assets:",
                  min = 0, 
                  max = max(mn_sample_geo$asset_amt, na.rm = TRUE),
                  value = 0, step = 10000),

    ),
    mainPanel(
      leafletOutput("nonprofitMap", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- mn_sample_geo
    
    data <- data %>%
      filter(revenue_amt >= input$revenue_range) %>%
      filter(asset_amt >= input$asset_range)
    
    data
  })
  
  
  output$nonprofitMap <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -94.6859, lat = 46.7296, zoom = 6)
  })
  
  observe({
    data <- filtered_data()
    leafletProxy("nonprofitMap", data = data) |> 
      addProviderTiles("CartoDB.Positron") |> 
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~popup_info,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })
}

shinyApp(ui, server)