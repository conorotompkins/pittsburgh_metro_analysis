#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    output$pgh_border_map <- renderPlot({
        source("pittsburgh_classification_map/munge_data.R")
        
        convex_hull <- threshhold_map %>% 
            filter(threshhold == input$threshhold_slider) %>% 
            group_by(flag_city, threshhold) %>% 
            summarize() %>% 
            filter(flag_city == TRUE) %>% 
            st_convex_hull() %>% 
            st_cast("LINESTRING")
        
        threshhold_map %>% 
            filter(threshhold == input$threshhold_slider) %>% 
            ggplot() +
            geom_sf(aes(fill = flag_city), color = NA, alpha = .8) +
            geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 1) +
            geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = .3) +
            geom_sf(data = convex_hull, color = "red") +
            #geom_sf(color = "black", fill = NA, size = 1) +
            #geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
            #facet_wrap(~threshhold) +
            scale_fill_viridis_d() +
            theme_void()
    
    })

})
