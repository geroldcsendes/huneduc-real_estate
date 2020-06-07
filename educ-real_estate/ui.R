library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
        
    dashboardHeader(title = "Budapest education and real-estate prices", titleWidth = 450),
    
    dashboardSidebar(width = 200,
        menuItem("Education", tabName = "educ"),
        menuItem("Real-estate prices", tabName = "house")
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "educ",
                    fluidRow(
                        box(sliderInput("year",
                                label = "Year:",
                                min = 2008,
                                max = 2018,
                                value = 2018,
                                sep = ""), width = 6, height = 100),
                        box(selectInput("district",
                                label = "Choose a district:",
                                choices = c("ALL", paste0(seq(1, 23), ". kerület")),
                                selected = "ALL"), width = 6, height = 100)
                    ),
                    fluidRow(
                        box(leafletOutput("mymap"), height = 400, width = 150),
                        box(dataTableOutput("table"), height = 300, width = 150)
                        )
            ),
            
            tabItem(tabName = "house",
                    fluidRow(
                        box(sliderInput("price",
                                label = "Price in million HUF:",
                                min = 10,
                                max = 100,
                                value = c(30, 35),
                                step = 10)),
                        box(selectInput("school", "Choose a school", multiple = F, choices = character(0), 
                                        selected = "Budapest V. Kerületi Eötvös József Gimnázium"))
                    ),
                    fluidRow(
                        box(leafletOutput("house_map"), height = 600, width = 150)
                    )
            )
        )
    )
)
