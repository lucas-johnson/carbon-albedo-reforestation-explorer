library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
library(shinyFeedback)
library(leaflet)
source("helpers.R")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    shinyFeedback::useShinyFeedback(),
    # App title ----
    title = "Reforestation Recommendations",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        numericInput(
            "lat",
            label = "Latitude",
            min = 22, 
            max = 50,
            step = 1,
            value = 43
        ),
        numericInput(
            "lon",
            label = "Longitude",
            min = -64, 
            max = -126,
            step = 1,
            value = -74
        )
    ),
    navset_card_underline(
        title = "Navigation",
        nav_panel("Map Selection", leafletOutput("map")),
        nav_panel("25-year Horizon", htmlOutput("top_three")),
        nav_panel("Timeseries", plotOutput("ts", width = '100%'))
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    conus_hex_shp <- sf::st_read("data/conus_hex.gpkg")
    
    lat_lon <- reactive({
        req(input$lat, input$lon)
        in_conus <- length(get_hex_id(conus_hex_shp, input$lat, input$lon)) != 0

        shinyFeedback::feedbackDanger("lat", !in_conus, "Lat/Lon not inside CONUS")
        shinyFeedback::feedbackDanger("lon", !in_conus, "Lat/Lon not inside CONUS")

        req(in_conus, cancelOutput = TRUE)

        return(list(lat = input$lat, lon = input$lon))
    })
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng = -98.5795, lat = 39.8283, zoom = 4) |> # center the map in USA
            addTiles(layerId = 'map_click') 
        
    })
    
    observeEvent(input$map_click, {
        click <- input$map_click
        lat <- click$lat
        lon <- click$lng
        
        proxy <- leafletProxy("map")
        updateNumericInput(session, "lat", value = lat)
        updateNumericInput(session, "lon", value = lon)
        
        in_conus <- length(get_hex_id(conus_hex_shp, lat, lon)) != 0
        
        shinyFeedback::feedbackDanger("lat", !in_conus, "Lat/Lon not inside CONUS")
        shinyFeedback::feedbackDanger("lon", !in_conus, "Lat/Lon not inside CONUS")
        req(in_conus, cancelOutput = TRUE)
    })
    
    output$ts <- renderPlot({
        render_ts(lat_lon())
    })

    output$top_three <- renderText({
        render_top_three_tab(lat_lon())
    })
    
}

shinyApp(ui = ui, server = server)