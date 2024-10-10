library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
source("helpers.R")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
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
        title = "25-year Recommendations",
        nav_panel("Table", htmlOutput("top_three")),
        nav_panel("Plot", plotOutput("ts", width = '100%'))
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    conus_hex_shp <- sf::st_read("data/conus_hex.gpkg")
    
    output$ts <- renderPlot({
        render_ts(input$lat, input$lon)
    })
    
    output$top_three <- renderText({
        render_top_three_tab(input$lat, input$lon)
    })
    
}

shinyApp(ui = ui, server = server)