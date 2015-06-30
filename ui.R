
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("init_db.R")

shinyUI(fluidPage(
  
  fluidPage(
    titlePanel("WDPA Data Explorer"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             selectInput("country", 
                         "Country:", 
                         c("All", 
                           unique(as.character(pa_per_iso3$iso3_country_name))))
      ),
      column(4, 
             selectInput("type", 
                         "Type:", 
                         c("All", 
                           c("polygon", "point")))
      ),
      column(4, 
             selectInput("iucn_cat", 
                         "IUCN category:", 
                         c("All", 
                           unique(as.character(pa_per_iso3$iucn_cat))))
      )
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId = "table")
    )    
  )  
))
