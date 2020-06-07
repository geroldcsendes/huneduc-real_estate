library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
source("global.R")

# print(getwd())

# define paths and filenames
data_path <- "../data/processed/"
okm_filename <- "okm_gps.csv"
okm_gps_filename <- "okm_distinct_gps.csv"
real_estate_filename <- "real_estate_df.csv"
# read in data
okm <- read.csv(paste0(data_path, okm_filename))
okm <- okm %>% arrange(omid, evfolyam)
coords_df <- read.csv(paste0(data_path, okm_gps_filename))
real_estate <- read.csv(paste0(data_path, real_estate_filename))


# func tryout
# mydf <- yearly_educ(year_sel = input$year, okm_df = okm, coords_df = coords_df)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataInput <- reactive({
        df <- yearly_educ(year_sel = input$year, district_sel = input$district, okm_df = okm, coords_df = coords_df)
        return(df)
    })
    
    output$mymap <- renderLeaflet({
        
        df <- dataInput()
        
        qpal <- colorNumeric("RdYlBu", df$educ_index)
        
        leaflet(df) %>% 
            
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 19.040236, lat = 47.497913, zoom = 11) %>% 
            addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = 250,
                       color = ~qpal(educ_index), fillOpacity = 0.8,
                       popup = ~as.character(nev_isk), label = ~as.character(nev_isk)) %>%
            addLegend("bottomright", pal = qpal, values = ~educ_index, 
                      title = "Educ", opacity = 0.85)
    })
    
    output$table <-  renderDataTable({
        df <- dataInput()
        df <- df %>%
            select(omid, nev_isk, irsz_isk, utca_isk, m_stat, o_stat, pooled_stat, educ_index) %>%
            arrange(-pooled_stat)

        df <- DT::datatable(df, extensions = c('Buttons','FixedHeader'), 
                            options = list(dom = 'Blfrtip',scrollX = TRUE, fixedHeader = TRUE,
                                           pageLength = 10,lengthMenu = c(10, 20, 50))) %>% 
            formatRound(columns=c('m_stat', 'o_stat', 'pooled_stat', 'educ_index'), digits=0)
    })

})
