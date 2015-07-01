
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

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
  
  # IUCN category
  output$iucncatPlot <- renderPlot({
    
    environment <- environment() 
    
    # Check what we are using on the y-axis
    if (input$yaxis == "Percent") {
      yaxis_string = "perc"
      suffix = " %"
    } else if (input$yaxis == "Count") {
      yaxis_string = "count"
      suffix = ""
    } else if (input$yaxis == "Area") {
      yaxis_string = "area_km"
      suffix = " km2"
    }
    
    # Copy global variables to local copies 
    iso3_data <- pa_per_iso3
    global_data <- pa_cat_global
    
    if (input$country != "All") {
      iso3_data <- iso3_data %>% 
        filter(iso3_country_name == input$country) %>%
        bind_rows(., global_data)
    } else {
      # Use just the global values
      iso3_data <- global_data
    }
    
    # Determine which types are used ("All", "poly", "point")
    if (input$type != "All") {
      iso3_data <- iso3_data %>% 
        filter(type == input$type)
    }
    
    # Calculate the stats dynamically, remember to group by iso3_country_name!
    iso3_data <- iso3_data %>% 
      group_by(iso3_country_name) %>% 
      mutate(perc = round(area_km / sum(area_km) * 100, 2)) %>% 
      select(iso3_country_name, iucn_cat, count, area_km, perc) %>% 
      ungroup()
    
    # Construct the plot
    p <- iso3_data %>% 
      ggplot(aes(x = iucn_cat, y = get(yaxis_string), fill = iso3_country_name),
             environment = environment) + 
      geom_bar(position = "dodge", stat = "identity") + 
      ylab(paste(input$yaxis, "\n")) + xlab("\nIUCN category") +
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                         legend.position = c(0, 1),
                         legend.justification = c(0, 1)) +
      scale_fill_discrete(name = "")
    
      if (input$yaxis == "Percent") {
        p <- p + ylim(0, 100)
      }
    
    output$summaryText <- renderText({
      
      # Create output text
      # Separate the country-level and global data
      if (input$country != "All") {
        
        print_iso3_data <- iso3_data %>% 
          filter(iso3_country_name != "Global")
        iso3_text <- paste0("Country total ", tolower(input$yaxis), ": \t", 
                            round(sum(print_iso3_data[yaxis_string]), 0), 
                            suffix, "\n")
      } else {
        iso3_text <- ""
      }
      
      print_global_data <- iso3_data %>% 
        filter(iso3_country_name == "Global")
      global_text <- paste0("Global total ", tolower(input$yaxis), ": \t", 
                          round(sum(print_global_data[yaxis_string]), 0), 
                          suffix)
      
      return(paste0(iso3_text, global_text))
      
    })
    
    return(p)
  })
  
  # Status
  output$statusPlot <- renderPlot({
    
    environment <- environment() 
    
    # Check what we are using on the y-axis
    if (input$yaxis == "Percent") {
      yaxis_string = "perc"
    } else if (input$yaxis == "Count") {
      yaxis_string = "count"
    } else if (input$yaxis == "Area") {
      yaxis_string = "area_km"
    }
    
    p <- pa_per_iso3 %>% 
      filter(iso3_country_name == filter_string) %>%
      mutate(perc = round(area_km / sum(area_km) * 100, 2)) %>% 
      select(iso3_country_name, status, count, area_km, perc) %>%
      bind_rows(., pa_stat_global) %>% 
      ggplot(aes(x = status, y = get(yaxis_string), fill = iso3_country_name),
             environment = environment) + 
      geom_bar(position = "dodge", stat = "identity") + 
      ylab(paste(input$yaxis, "\n")) + xlab("\nIUCN category") +
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                         legend.position = c(0, 1),
                         legend.justification = c(0, 1)) +
      scale_fill_discrete(name = "")
    
      if (input$yaxis == "Percent") {
        p <- p + ylim(0, 100)
      }
      return(p)
    
  })
})
