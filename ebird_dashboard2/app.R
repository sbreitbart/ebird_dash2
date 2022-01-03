# load libraries-----
library(shiny)
library(shinydashboard)
library(leaflet)
library(janitor)
library(tidyverse)
library(magrittr)
library(extrafont)
library(scales)
library(shinyWidgets)
library(DT)
library(reactable)
library(htmltools)
library(backports)


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

# join families with ebird data
ebird %<>%
    left_join(ebird_codes, by = "taxonomic_order")


# find # of birds seen per month
ebird_monthly <- ebird %>%
    tidyr::drop_na(date) %>%
    dplyr::mutate(count_est = replace_na(count, 1)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(count_est = as.numeric(count_est)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(count_est = sum(count_est)) %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::select(-date) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(count_est = sum(count_est)) %>%
    dplyr::mutate(total = cumsum(count_est))

# add seasons to df
months <- as.numeric(format(as.Date(ebird_monthly$month, '%m/%d/%Y'), '%m'))
indx <- setNames( rep(c('winter', 'spring', 'summer',
                        'fall'),each=3), c(12,1:11))
ebird_monthly$Season <- unname(indx[as.character(months)])

# find 20 most commonly-seen bird species
ebird_top20 <- ebird %>%
    group_by(scientific_name.x, common_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(20)


ebird_singles <- ebird %>%
    group_by(scientific_name.x) %>%
    summarise(n = n(),
              common_name = first(common_name),
              location = first(location),
              lat = first(latitude),
              long = first(longitude),
              date = first(date),
              comments = first(checklist_comments),
              details = first(observation_details)) %>%
    filter(n == 1) %>%
    # remove some observations only at the genus level
    filter(!str_detect(scientific_name.x, 'sp.')) %>%
    filter(!str_detect(scientific_name.x, 'Group')) %>%
    dplyr::mutate(year = substr(date, 1, 4))


# rarest birds table
rarest_birds1 <- ebird_singles %>%
    dplyr::select(c(1,3,4,7,9,10)) %>%
    dplyr::rename("Common Name" = 2) %>%
    dplyr::rename("Scientific Name" = 1) %>%
    dplyr::rename("Location" = 3) %>%
    dplyr::rename("Date" = 4) %>%
    dplyr::rename("Comment" = 5) %>%
    dplyr::rename("Year" = 6) %>%
    dplyr::mutate("Link" = paste0(
        "https://www.google.com/search?q=",
        ebird_singles$common_name,
        sep = " ")) %>%
    dplyr::mutate(Date = as.Date(Date))

                 
ebird_monthly_all <- ebird %>%
    tidyr::drop_na(date) %>%
    dplyr::mutate(count_est = replace_na(count, 1)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(count_est = as.numeric(count_est)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(count_est = sum(count_est)) %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::select(-date) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(count_est = sum(count_est)) %>%
    dplyr::mutate(month_num = format(month,"%m")) %>%
    dplyr::mutate(month_str = format(month,"%B")) %>%
    dplyr::group_by(month_str, month_num) %>%
    dplyr::summarise(total = sum(count_est)) %>%
    arrange(month_num)



monthly_birds <- ebird %>%
    tidyr::drop_na(date) %>%
    dplyr::mutate(count_est = replace_na(count, 1)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(count_est = as.numeric(count_est)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(count_est = sum(count_est)) %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::select(-date) %>%
    dplyr::mutate(month_num = format(month,"%m")) %>%
    dplyr::mutate(month_str = format(month,"%B")) %>%
    ggplot( aes(x = month_str,
                y = count_est,
                fill = month_str,
                group = month_str)) +
    geom_boxplot() +
    scale_x_discrete(limits = month.name) +
    geom_jitter(aes(fill = month_str), color = "black", shape = 21, size=1.2, alpha=0.9) +
    theme_bw() +
    theme(
        legend.position="none",
        plot.title = element_text(size=11)
    ) +
    scale_fill_brewer(type = "qual", palette = "Set3") +
    xlab("") +
    labs(y = "Birds Seen per Outing")


# best comments
ebird_comments <- ebird %>%
    dplyr::filter(observation_details != "") %>%
    dplyr::slice(c(9, 20, 21, 22, 34, 40, 48, 58, 59, 66, 68, 78, 81, 84, 86, 105, 108, 110, 113, 115, 117, 122))


leaflet(ebird_comments) %>% 
    # addTiles(options = providerTileOptions(opacity = 0.55)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    #                  options = providerTileOptions(opacity = 0.55)) %>%
    # fitBounds(-79.6,44,-79.2,43.5) %>% 
    addCircles(lng = ~longitude,
               lat = ~latitude,
               radius = 20,
               popup = paste0("Scientific name: ",
                              # italicize
                              "<i>",
                              ebird_comments$scientific_name.x,
                              "</i>",
                              ".",
                              "<br>", # line break
                              "Common name: ",
                              ebird_comments$common_name, 
                              ".",
                              "<br>", # line break
                              "Location: ",
                              ebird_comments$location,
                              ".",
                              "<br>", # line break
                              "Date: ",
                              ebird_comments$date,
                              ".",
                              "<br>", # line break
                              "Comment: ",
                              ebird_comments$observation_details,
                              "."),
               fill = T,
               fillOpacity = 0.8
    ) 





# make color palette for US sightings map
pal <- colorFactor(palette = 'Dark2',
                    domain = ebird$year)

# make color palette for NZ sightings map
pal2 <- colorFactor(palette = 'Set1',
                   domain = ebird$family)


# loadfonts(device = "win")





ui <- fluidPage(
    titlePanel("Mitch's Bird Sightings"),  # Add a title panel
    sidebarLayout(  # Make the layout a sidebarLayout
        sidebarPanel(
            # selectInput(inputId = "year",
            #             label = h3("Select Year"),
            #             choices = c(
            #                 "2016" = 2016,
            #                 "2017" = 2017,
            #                 "2018" = 2018,
            #                 "2019" = 2019,
            #                 "2020" = 2020,
            #                 "2021" = 2021)
            # ),
            shinyWidgets::pickerInput(inputId = "family",
                         label = h3("Select Family (US Map)"),
                         choices = as.list(
                             sort(na.exclude(unique(ebird$family)))),
                        selected = "Accipitridae (Hawks, Eagles, and Kites)",
                        options = list(`actions-box` = TRUE,
                                       `selected-text-format` = "count > 2"),
                        multiple = TRUE)
        ),  # Inside the sidebarLayout, add a sidebarPanel
       
         mainPanel( tabsetPanel(
            type = "tabs",
            
            tabPanel("US Sightings",
                     fluidPage(leafletOutput("US_sightings"),
                               DT::dataTableOutput('US_table'))
                     ),
            
            tabPanel("New Zealand Sightings",
                     fluidPage(leafletOutput("NZ_sightings"),
                               DT::dataTableOutput('NZ_table'))
                     ),
            
            tabPanel("Sightings Over Time",
                     plotOutput("seasons_plot")),
            
            tabPanel("Most Popular Birds",
                     plotOutput("common20")),
            
            tabPanel("Rarest Birds",
                     fluidPage(
                     reactableOutput("rarest_birds"))),
            
            tabPanel("Sightings by Month",
                     plotOutput("monthly_birds")),
            
            tabPanel("Sightings with the Best Comments",
                     plotOutput('bird_comments'))
                        ) 
    ))
)



server <- function(input, output, session) {
    
    # FIRST MAP: ALL US SIGHTINGS
    ebird_US <- reactive({
        ebird %>%
            filter(state_province != "NZ-OTA") %>%
            filter(family == input$family)
        # %>%
        #     filter(year == input$year)
                   })
    
    output$US_table <- DT::renderDataTable(
        ebird %>%
            filter(state_province != "NZ-OTA") %>%
            dplyr::select(c(2,3,5,9, 12)) %>%
            dplyr::rename("Common Name" = 1) %>%
            dplyr::rename("Scientific Name" = 2) %>%
            dplyr::rename("Count" = 3) %>%
            dplyr::rename("Location" = 4) %>%
            dplyr::rename("Date" = 5))
    
    
    output$US_sightings <- renderLeaflet(
        leaflet() %>%
            addTiles(
                options = providerTileOptions(opacity = 0.55)
                     ) %>%
            # addProviderTiles(providers$Stamen.Toner) %>%
            addCircles(data = ebird_US(),
                       lng = ~longitude,
                       lat = ~latitude,
                       # radius = 200,
                       popup = paste0("Scientific name: ",
                                      # italicize
                                      "<i>",
                                      ebird_US()$scientific_name.x,
                                      "</i>",
                                      ".",
                                      "<br>", # line break
                                      "Common name: ",
                                      ebird_US()$common_name, 
                                      ".",
                                      "<br>", # line break
                                      "Location: ",
                                      ebird_US()$location,
                                      ".",
                                      "<br>", # line break
                                      "Date: ",
                                      ebird_US()$date,
                                      "."),
                       fill = T,
                       fillOpacity = 0.8,
                       color = ~pal(year)
            )
        %>%
            addLegend("bottomleft",
                      pal = pal,
                      values = ebird$year,
                      opacity = 0.8)
    )   
    
    
    
    # 2nd MAP: ALL NEW ZEALAND SIGHTINGS
    ebird_NZ <- ebird %>%
        filter(state_province == "NZ-OTA") # %>%
            # filter(family == input$family)
    
    output$NZ_table <- DT::renderDataTable(
        ebird_NZ %>%
            dplyr::select(c(2,3,5,9, 12)) %>%
            dplyr::rename("Common Name" = 1) %>%
            dplyr::rename("Scientific Name" = 2) %>%
            dplyr::rename("Count" = 3) %>%
            dplyr::rename("Location" = 4) %>%
            dplyr::rename("Date" = 5))
    
    output$NZ_sightings <- renderLeaflet(
        leaflet() %>%
            addTiles(
                options = providerTileOptions(opacity = 0.55)
            ) %>%
            # addProviderTiles(providers$Stamen.Toner) %>%
            addCircles(data = ebird_NZ,
                       lng = ~longitude,
                       lat = ~latitude,
                       radius = 900,
                       popup = paste0("Scientific name: ",
                                      # italicize
                                      "<i>",
                                      ebird_NZ$scientific_name.x,
                                      "</i>",
                                      ".",
                                      "<br>", # line break
                                      "Common name: ",
                                      ebird_NZ$common_name, 
                                      ".",
                                      "<br>", # line break
                                      "Location: ",
                                      ebird_NZ$location,
                                      ".",
                                      "<br>", # line break
                                      "Date: ",
                                      ebird_NZ$date,
                                      "."),
                       fill = T,
                       fillOpacity = 0.8,
                       color = ~pal2(family)
            ) 
        # %>%
        #     addLegend("bottomleft",
        #               pal = pal,
        #               values = ebird$family,
        #               opacity = 0.8)
    ) 
    
    # BARPLOT
    output$seasons_plot <-renderPlot(
        ggplot(ebird_monthly %>%
                               filter(total > 12),
                           aes(x = month,
                               y = total))+ 
        geom_bar(stat = "identity",
                 aes(fill = Season)) +
        # geom_smooth(method = "loess",se = F) + 
        theme_classic() +
        theme(legend.position = c(0.1,0.8),
              text = element_text(size = 14,
                                  family = "Segoe UI Semilight")) +
        labs(x = "Month",
             y = "Total Birds Recorded") +
        scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y")
    )
    
    # LOLLIPOP GRAPH: MOST COMMONLY-SEEN BIRDS
    output$common20 <- renderPlot(
        ggplot(ebird_top20,
               aes(
                   x = reorder(common_name, -count),
                   y = count)) +
            # first time so that x axis stays sorted by size
            geom_point(aes(color = reorder(common_name, -count)),
                       size = 5) +
            geom_segment( aes(x=common_name,
                              xend=common_name,
                              y=0,
                              yend=count),
                          color="skyblue",
                          size = 1) +
            # second time so that the points are in front of segments
            geom_point(aes(color = reorder(common_name, -count)),
                       size = 5) +
            theme_light() +
            ylim(0, 210) +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 35,
                                             vjust = 1.1,
                                             hjust= 1,
                                             size = 12),
                  panel.grid.major.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.border = element_blank(),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  plot.title = element_text(size=16,
                                            hjust = 0.5)) +
            labs(x = "Species",
                 y = "Times Seen (of 205 outings)",
                 title = "Mitch's 20 Most Spotted Birds",
                 subtitle = "When Mitch goes birding, he is most likely to see these species.") +
            geom_text(aes(label = count),
                      position = position_dodge(width=0.9),
                      vjust = -0.8)
    
    )

    output$rarest_birds <- renderReactable({
        reactable::reactable(rarest_birds1,
                             groupBy = "Year",
                             columns = list(
                                 Link = colDef(html = FALSE,
                    cell = function(value) {
                    htmltools::tags$a(href = value,
                                      target = "_blank",
                                      value)})                    ),
                    highlight = TRUE
                    
                    )
        })
 
    # birds seen per month
    output$monthly_birds <- renderPlot(
        monthly_birds)
    
    
    # best comments
    
       
}






shinyApp(ui, server)
