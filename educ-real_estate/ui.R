library(shiny)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Hungarian education and real-estate prices"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # TODO make this look good
            sliderInput("year",
                        label = "Year:",
                        min = 2008,
                        max = 2018,
                        value = 2018,
                        sep = ""),
            selectInput("district",
                        label = "Choose a district:",
                        choices = c("ALL", paste0(seq(1, 23), ". kerület")),
                        selected = "ALL")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap"),
            dataTableOutput("table")
        )
    )
))
