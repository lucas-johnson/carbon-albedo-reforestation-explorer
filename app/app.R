library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
library(shinyFeedback)
library(leaflet)
library(waiter)
source("helpers.R")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
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
        nav_panel("25-year Horizon", htmlOutput("rank_table")),
        nav_panel("Timeseries", plotOutput("ts", width = '100%'))
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    conus_hex_shp <- sf::st_read("data/conus_hex.gpkg", quiet = TRUE)
    hex_conditions <- read.csv("data/hex_conditions.csv")
    hex_ids <- hex_conditions |>
        dplyr::pull(USHEXES_ID) |> 
        unique()
    c_x_albedo <- read.csv("data/c_albedo_table.csv")
    
    any_forest <- reactive({
        get_hex_id(conus_hex_shp, input$lat, input$lon) %in% hex_ids
    })
    
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
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        
        coords <- lat_lon()
        
        if(!any_forest()) {
            validate("Location currently nonforested.")
        }
        
        render_ts(coords, conus_hex_shp, hex_conditions, c_x_albedo)
    })

    output$rank_table <- renderText({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        
        coords <- lat_lon()
        
        if(!any_forest()) {
            validate("Location currently nonforested.")
        }
        
        render_rank_tab(coords, conus_hex_shp, hex_conditions, c_x_albedo)
    })
    
}

shinyApp(ui = ui, server = server)