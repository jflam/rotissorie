library(shiny)
library(leaflet)

shinyUI(fluidPage(
    titlePanel("Predicted stats for hitters by position"),

    sidebarLayout(
        sidebarPanel(
            helpText("Show predicted stats for hitter"),
            uiOutput("controls")
        ),
        mainPanel(

            # This is where the generated table lives
            dataTableOutput("table"),
            dataTableOutput("starters"),
            dataTableOutput("closers")
        )
    )
))