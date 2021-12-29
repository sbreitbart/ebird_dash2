# load libraries-----
library(shiny)
library(shinydashboard)
library(leaflet)
library(janitor)
library(tidyverse)
library(magrittr)

# import data----
ebird <- read.csv("ebird.csv") %>%
    janitor::clean_names() %>%
    dplyr::mutate(count = na_if(count, "X")) %>%
    dplyr::mutate(year = substr(date, 1, 4)) %>%
    as.data.frame()

ebird_codes <- read.csv("spcs_code.csv") %>%
    janitor::clean_names() %>%
    rename("taxonomic_order" = 1) %>%
    as.data.frame()

ebird %<>%
    left_join(ebird_codes, by = "taxonomic_order")

# make color palette for all sightings map
pal2 <- colorFactor(palette = 'Spectral',
                    domain = ebird$family)


ui <- fluidPage(
    titlePanel("Mitch's Bird Sightings"),  # Add a title panel
    sidebarLayout(  # Make the layout a sidebarLayout
        sidebarPanel(
            selectInput(inputId = "year",
                        label = h3("Select Year"),
                        choices = c(
                            "2016" = 2016,
                            "2017" = 2017,
                            "2018" = 2018,
                            "2019" = 2019,
                            "2020" = 2020,
                            "2021" = 2021)
            ),
            selectInput("select",
                         label = h3("Select Family"),
                         choices = as.list(
                             sort(na.exclude(unique(ebird$family)))))
        ),  # Inside the sidebarLayout, add a sidebarPanel
       
         mainPanel( tabsetPanel(
            type = "tabs",
            
            tabPanel("US Sightings",
                     plotOutput("pointPlot")),
            
            tabPanel("New Zealand Sightings",
                     plotOutput("countyPlot")),
            
            tabPanel("Sightings Over Time",
                     plotOutput("birdsOverTime")),
            
            tabPanel("Most Popular Birds",
                     plotOutput("blockBirds")),
            
            tabPanel("Rarest Birds",
                     plotOutput("LifebirdsState")),
            
            tabPanel("Sightings by Month",
                     plotOutput("birdsByMonth")),
            
            tabPanel("Sightings with the Best Comments",
                     plotOutput('birdsByYear'))
            
            # leafletOutput("plot")
            )  # Inside the sidebarLayout, add a mainPanel
    ))
)



server <- function(input, output, session) {
    
    # FIRST MAP: RARE SIGHTINGS
    ebird_filtered <- reactive({
        ebird %>%
            filter(year == input$year)})
    
    output$plot <- renderLeaflet(
        leaflet() %>%
            addTiles(options = providerTileOptions(opacity = 0.55)) %>%
            addCircles(data = ebird_filtered(),
                       lng = ~long,
                       lat = ~lat,
                       radius = 200,
                       popup = paste0("Scientific name: ",
                                      # italicize
                                      "<i>",
                                      ebird_filtered()$scientific_name,
                                      "</i>",
                                      ".",
                                      "<br>", # line break
                                      "Common name: ",
                                      ebird_filtered()$common_name, 
                                      ".",
                                      "<br>", # line break
                                      "Location: ",
                                      ebird_filtered()$location,
                                      "."),
                       fill = T,
                       fillOpacity = 0.8,
                       color = ~pal(year)
            ) %>%
            addLegend("bottomleft",
                      pal = pal,
                      values = ebird_singles$year,
                      opacity = 0.8)
    )                     
}


shinyApp(ui, server)
