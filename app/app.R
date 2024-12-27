library(shiny)
library(bslib)
library(kableExtra)
library(ggplot2)
library(shinyFeedback)
library(leaflet)
source("helpers.R")


ui <- page_fluid(
    # App title ----
    title = "Carbon and Albedo Reforestation Explorer",
    navset_card_underline(
        title = "Carbon and Albedo Reforestation Explorer",
        nav_panel("Application", 
                  card(card_header("Hexagon Selection"),
                       card_body(leafletOutput("map")),
                       card_footer(htmlOutput("notice"))),
                  card(card_header("25-year Horizon"), 
                       card_body(htmlOutput("rank_table"))),
                  card(card_header("Timeseries"), 
                       card_body(plotOutput("ts")))),
        nav_panel("Documentation", 
                  card(htmlOutput("docs"))),
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
            result <- "Zoom into the map and click on a hexgon to generate results."
        } else if (!hex_id() %in% hex_ids) {
            result <- "<span style=color:#C41E3A>Selected hexagon is not currently forested. Make a new selection.</span>"
        } else {
            result <- "Zoom into the map and click on a hexgon to generate results."
        }
        return(result)
    })
    
    
    output$docs <- renderUI({
        
        
        
        return(list(
            h2("CARE - the Carbon and Albedo Reforestation Explorer"),
            strong("v1.0.0"),
            div("Our application is built for managers interested in planting optimal forest-type groups for climate change mitigation outcomes approaching the year 2050. We assist in one of the most fundamental steps in any reforesation project: deciding which forest type or species mix to plant. We used a new carbon storage and albedo offset dataset in concert with FIA species distribution data to answer these questions. This application ranks available FIA-classified forest-type groups on the basis of joint C-albedo (non-soil carbon storage less a carbon-equivalent albedo offset) in a local area of interest. We intend for managers to combine this information with their unique understanding of project- or site specific constraints and goals."),
            h2("Methods"),
            strong("The Forest Inventory and Analysis Database"),
            div("The United States (US) Forest Inventory and Analysis program (FIA) maintains a nationwide network of permanent inventory plots arranged in a quasi-systematic hexagonal grid with a sample density of one plot for every ~2,400 ha (Bechtold and Patterson, 2005). The inventory records land uses, and within forested plots collects measurements of live and dead trees, litter, downed wood, and soil, as well as observations of stand-age and classifications into 30 forest-type groups. These measurements are later translated into estimates of biomass and carbon for five distinct pools (aboveground and belowground biomass, deadwood, litter, soil), are made available in a public relational database (Gray et al., 2012), and ultimately provide the basis for national forest carbon accounting (Woodall et al., 2015; Domke et al., 2023)."),
            strong("Joint C-albedo estimates by forest-type and age groups"),
            div("Healey et al. (In review) applied standard FIA estimation methods to develop tables of non-soil carbon storage for the Inventory’s 30 distinct forest-type groups, indexed by 10-year age bins. Although FIA has since transitioned to using a new system of allometric equations to convert granular measurements (e.g. di- ameter, height) to tree-level estimates of biomass and carbon (Westfall et al., 2024), Healey et al. (In review) derived estimates based on the component-ratio-method (Woodall et al., 2011) which was the national stan- dard at that time. Corresponding carbon-equivalent albedo impacts were identified for each type-by-age group using FIA’s condition-level expansion factors for weights. Albedo impact for every FIA plot was determined by the annualized albedo difference, assessed through 4 years of Landsat albedo measurements (Erb et al., 2022), between the plot and its ten nearest non-forest plots. Albedo differences were converted to a radiative forcing through the use of a radiative kernel (Bright and O’Halloran, 2019), and annualized radiative forcings were converted to carbon-equivalent units (Bright et al., 2016). Refer to Healey et al. (In review) for methodological details and Healey and Yang (2023) for resulting forest-type group-specific carbon stocks and albedo impacts."),
            strong("Ranking forest-type groups within 64,000 ha hexagons"),
            div("We usd a CONUS-wide hexagonal tessellation of 64,000 hectare hexagons to explore spatial patterns of albedo impacts on planting decisions. These hexagons, which are sometimes used for fine- resolution analysis of the FIA database (Menlove and Healey, 2020), each contain ~27 FIA plots. Within each hexagon we identified the set of forest-type groups present in the most recent panel of inventories from each state (Bechtold and Patterson, 2005), and ranked each forest-type group at the 20-30 year-old bin (25-year) on the basis of both joint C-albedo benefits (non-soil carbon stock less a carbon-equivalent albedo offset) and non-soil carbon storage. Additionally, we present 100-year chronosequences (10-year bins) for each attribute (carbon, albedo offsets, joint C-albedo) across the same set of forest-type groups to offer insight into the temporal consistency of joint C-albedo benefits."),
            p("See Johnson et al., (in review) for a full description of the methods used to generate the data displayed in this application."),
            h2("Limitations"),
            div("The estimates of joint C-albedo benefits that support our application were made under climatic and atmospheric conditions that are unlikely to remain constant in the next 25 years. Specifically, we relied on chronosequences of non-soil carbon stocks estimated from historical inventory data measured as recently as 2021, thus reflecting forest establishment and growth under the climate of the ~30 preceding years. Similarly, we used carbon-equivalent albedo offset estimates made with Landsat 8 imagery collected between 2016 and 2019, thus reflecting the atmospheric conditions and earth surface properties from this particular time frame (Healey et al., In review).  Given the dynamic, and likely interrelated, forecasted shifts in global climate, atmospheric composition, and surface properties, we emphasize the uncertainty surrounding the use of current conditions as a proxy for future climate impacts."),
            h2("Support"),
            div("If you encounter any problems, or have any recommendations for how to improve the app's functionality, please submit a",
                tags$a("github issue", href="https://github.com/lucas-johnson/reforestation-recommendations/issues", target = "_blank"),
                "or reach out to the app developers using the contact information provided below."),
            h2("Citation"),
            p("Johnson, Yang, Erb, Bright, Domke, Frescino, Schaaf, Healey, (in review), Integrating albedo offsets in reforestation decisions for climate change mitigation outcomes in 2050: a case study in the USA."),
            h2("References"),
            p("Bright, Bogren, Bernier, Astrup, (2016). Carbon-equivalent metrics for albedo changes in land management contexts: Relevance of the time dimension. Ecol. Appl. 26, 1868–1880"),
            p("Bright, R. M., & O'Halloran, T. L. (2019). Developing a monthly radiative kernel for surface albedo change from satellite climatologies of Earth's shortwave radiation budget: CACK v1. 0. Geoscientific Model Development, 12(9), 3975-3990."),
            div("Domke, Walters, Nowak, Greenfield, Smith, Nichols, Ogle, Coulston, Wirth (2022). Greenhouse Gas Emissions and Removals From Forest Land, Woodlands, Urban Trees, and Harvested Wood Products in the United States, 1990–2020. (US Dept. Ag. For. Service, Madison, WI;",
                tags$a("https://doi.org/10.2737/FS-RU-382", href = "https://doi.org/10.2737/FS-RU-382", target = "_blank"),
                ")."),
            p("Erb, Li, Sun, Paynter, Wang, & Schaaf, (2022). Evaluation of the Landsat-8 Albedo Product across the Circumpolar Domain. Remote Sensing, 14(21), 5320."),
            p("Healey, Yang, Erb, Bright, Domke, Frescino, Schaaf, (in review) New satellite observations expose albedo dynamics offsetting half of carbon storage benefits in US forests."),
            div("Healey, S., & Yang, Z. (2023). Carbon storage and carbon-equivalent albedo impact for US forests, by age and forest type [Data set]. Zenodo.",
                tags$a("https://doi.org/10.5281/zenodo.8320433", href = "https://doi.org/10.5281/zenodo.8320433", target="_blank")),
            h2("Contact information"),
            div(tags$a("Lucas Johnson", href="https://lucaskjohnson.com", target="_blank"),
                "- johnsl27@oregonstate.edu"),
            div("Zhiqiang Yang - zhiqiang.yang@usda.gov"),
            div(tags$a("Sean Healey", href="https://orcid.org/0000-0003-3498-4266", target="_blank"), 
                "- sean.healey@usda.gov")
        ))
    })
    
}

shinyApp(ui = ui, server = server)