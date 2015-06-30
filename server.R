
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
  
  # Fill in the spot we created for a plot
  output$iucncatPlot <- renderPlot({
    
    environment <- environment() 
    
    # If country is set to "All", display just the global stuff
    if (input$country == "All") {
      filter_string = "*"
    } else {
      filter_string = input$country
    }
    
    # Check what we are using on the y-axis
    if (input$yaxis == "Percent") {
      yaxis_string = "perc"
    } else if (input$yaxis == "Count") {
      yaxis_string = "count"
    } else if (input$yaxis == "Area") {
      yaxis_string = "area_km"
    }
    
    
    
    pa_per_iso3 %>% 
      filter(iso3_country_name == filter_string) %>%
      mutate(perc = round(area_km / sum(area_km) * 100, 2)) %>% 
      select(iso3_country_name, iucn_cat, count, area_km, perc) %>%
      bind_rows(., pa_global) %>% 
      ggplot(aes(x = iucn_cat, y = get(yaxis_string), fill = iso3_country_name),
             environment = environment) + 
      geom_bar(position = "dodge", stat = "identity") + 
      ylab(paste(input$yaxis, "\n")) + xlab("\nIUCN category") +
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                         legend.position = c(0, 1),
                         legend.justification = c(0, 1)) +
      scale_fill_discrete(name = "")
  })
})
