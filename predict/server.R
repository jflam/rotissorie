library(shiny)
library(DT)

# This code runs exactly once. We call the function to compute the model

source("baseball_knn.R")
positions = c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "OF")

shinyServer(function(input, output) {

    # We dynamically generate a listbox control for the client that 
    # contains the list of countries sorted alphabetically

    output$controls <- renderUI({
        selectInput("country",
            label = "Select a position",
            choices = positions, 
            selected = "C"
        )
    })

    output$table <- renderDataTable({
        prediction
    })
})