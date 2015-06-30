
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

#  Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    data <- pa_per_iso3
    if (input$country != "All"){
      data <- data[data$iso3_country_name == input$country,]
    }
    if (input$type != "All"){
      data <- data[data$type == input$type,]
    }
    if (input$iucn_cat != "All"){
      data <- data[data$iucn_cat == input$iucn_cat,]
    }
    if (input$status != "All"){
      data <- data[data$status == input$status,]
    }
    data <- data %>% 
      group_by(iso3_country_name) %>% 
      mutate(percent = round(area_km / sum(area_km) * 100, 2)) %>% 
      arrange(iso3_country_name, iucn_cat)
    return(data)
  })
})
