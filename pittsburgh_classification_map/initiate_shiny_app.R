library(shiny)

source("pittsburgh_classification_map/ui.R")
source("pittsburgh_classification_map/server.R")

shiny::shinyApp(ui, server)

