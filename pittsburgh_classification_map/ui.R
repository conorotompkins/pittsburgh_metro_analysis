#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Pittsburgh Border Map"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("threshhold_slider",
                        "Threshhold probability:",
                        min = .1,
                        max = .9,
                        value = .5,
                        step = .1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pgh_border_map")
        )
    )
))
