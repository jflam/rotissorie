library(shiny)
library(leaflet)

shinyUI(fluidPage(
    titlePanel("Predicted stats for hitters by position"),

    sidebarLayout(
        sidebarPanel(
            helpText("Show predicted stats for hitter"),

            # Country selector control is generated on the server 
            # and sent to client

            uiOutput("controls")
        ),
        mainPanel(

            # This is where the generated table lives

            leafletOutput("table")
        )
    )
))