library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
library(shinyFeedback)
library(leaflet)
source("helpers.R")


ui <- page_fluid(
    # App title ----
    title = "Reforestation Recommendations",
    navset_card_underline(
        title = "Reforestation Recommendations",
        nav_panel("Application", 
                  card(card_header("Hexagon Selection"),
                       card_body(leafletOutput("map")),
                       card_footer(htmlOutput("notice"))),
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
    hex_id <- reactiveVal(NULL)
    selected <- reactiveVal(NULL)
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng = -98.5795, lat = 39.8283, zoom = 4) |> # center the map in USA
            addTiles(layerId = 'map_click') |>
            addPolygons(data = conus_hex_shp,
                        layerId = conus_hex_shp$USHEXES_ID,
                        group = 'base',
                        fillColor = NA,
                        opacity = 1, 
                        color = 'black', 
                        fillOpacity = 0, 
                        weight = 0.4, 
                        highlightOptions = highlightOptions(color = "#CC0000", 
                                                            weight = 3, 
                                                            bringToFront = TRUE))
        
    })
    
    observeEvent(input$map_click, {
        
        proxy <- leafletProxy("map")
        
        new_selected <- req(input$map_shape_click, cancelOutput = TRUE)
        old_selected <- selected()

        if (is.null(old_selected) || new_selected$.nonce != old_selected$.nonce) {
            validate(
                need(new_selected$group!="selection", message=FALSE)
            )
            
            hex_id(new_selected$id)
            selected(new_selected)
            
            i <- which(conus_hex_shp$USHEXES_ID==new_selected$id)
            conus_hex_shp_filtered <- conus_hex_shp[i,]

            proxy |>
                clearGroup("selection") |>
                addPolygons(
                    data = conus_hex_shp_filtered,
                    group = 'selection',
                    fillColor = "cyan",
                    weight = 1.2,
                    color = "black",
                    fillOpacity = 0.6)
        } 
    
    })
    
    output$ts <- renderPlot({
        req(hex_id() %in% hex_ids)
        
        render_ts(hex_id(), conus_hex_shp, hex_conditions, c_x_albedo)
    })

    output$rank_table <- renderText({
        req(hex_id() %in% hex_ids)
        
        render_rank_tab(hex_id(), conus_hex_shp, hex_conditions, c_x_albedo)
    })
    
    output$notice <- renderText({
        if(is.null(hex_id()) | length(hex_id()) == 0) {
            result <- "Use map to select a hexagon. Then scroll down for results."
        } else if (!hex_id() %in% hex_ids) {
            result <- "<span style=color:#C41E3A>Selected hexagon is not currently forested. Make a new selection.</span>"
        } else {
            result <- "Use map to select a hexagon. Then scroll down for results."
        }
        return(result)
    })
    
}

shinyApp(ui = ui, server = server)