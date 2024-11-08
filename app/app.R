library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
library(shinyFeedback)
library(leaflet)
library(waiter)
source("helpers.R")


ui <- page_fluid(
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    # App title ----
    title = "Reforestation Recommendations",
    navset_card_underline(
        title = "Reforestation Recommendations",
        nav_panel("Application", 
                  card(card_header("Hexagon Selection"),
                       card_body(leafletOutput("map")),
                       card_footer(htmlOutput("selection_info"))),
                  card(card_header("25-year Horizon"), 
                       card_body(htmlOutput("rank_table"))),
                  card(card_header("Timeseries"), 
                       card_body(plotOutput("ts")))),
        nav_panel("Documentation", 
                  card(card_body(p("Here is where we will describe the app, the underlying data and methods, and link to some references.")))),
    )

)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    conus_hex_shp <- sf::st_read("data/conus_hex.geojson", quiet = TRUE)
        
    hex_conditions <- read.csv("data/hex_conditions.csv")
    hex_ids <- hex_conditions |>
        dplyr::pull(USHEXES_ID) |> 
        unique()
    c_x_albedo <- read.csv("data/c_albedo_table.csv")
    lat_lon <- reactiveValues()
    in_conus <- reactiveVal()
    any_forest <- reactiveVal()
    
    check_lat_lon <- reactive({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        req(lat_lon$lat, lat_lon$lon, cancelOutput = TRUE)
        
        hex_id <- get_hex_id(conus_hex_shp, lat_lon$lat, lat_lon$lon)
        in_conus(length(hex_id) != 0)
        any_forest(hex_id %in% hex_ids)
        
        req(in_conus(), cancelOutput = TRUE)
        req(any_forest(), cancelOutput = TRUE)
        message(in_conus())
        message(any_forest())
        
        return(lat_lon)
    })
    
    output$map <- renderLeaflet({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        leaflet() %>% 
            setView(lng = -98.5795, lat = 39.8283, zoom = 4) |> # center the map in USA
            addTiles(layerId = 'map_click') |>
            addPolygons(data = conus_hex_shp,
                        fillColor = NA,
                        opacity = 1, 
                        color = 'black', 
                        fillOpacity = 0, 
                        weight = 0.4)
        
    })
    
    observeEvent(input$map_click, {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        proxy <- leafletProxy("map")
        click <- input$map_click
        lat_lon$lat <- input$map_click$lat
        lat_lon$lon <- input$map_click$lng
        check_lat_lon()
    })
    
    output$ts <- renderPlot({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        req(lat_lon$lat, lat_lon$lon, in_conus(), any_forest(), cancelOutput = TRUE)
        render_ts(lat_lon, conus_hex_shp, hex_conditions, c_x_albedo)
    })

    output$rank_table <- renderText({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        req(lat_lon$lat, lat_lon$lon, in_conus(), any_forest(), cancelOutput = TRUE)
        render_rank_tab(lat_lon, conus_hex_shp, hex_conditions, c_x_albedo)
    })
    
    output$selection_info <- renderText({
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        if (is.null(lat_lon$lat) || is.null(in_conus()) || is.null(any_forest())) {
            result <- "Use map to select a hexagon. Then scroll down for results."
        } else if (!in_conus()) {
            result <- "Use map to select a hexagon. Then scroll down for results."
        } else if (!any_forest()) {
            result <- "<span style=color:#C41E3A>Selected hexagon is not currently forested. Make a new selection.</span>"
        } else {
            result <- glue::glue("Lat: {round(lat_lon$lat, 4)}, Lon: {round(lat_lon$lon, 4)}")
        }
        return(result)
    })
    
}

shinyApp(ui = ui, server = server)