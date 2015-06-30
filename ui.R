
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
      fluidRow(
        column(8,
          # Create a new Row in the UI for selectInputs
          fluidRow(
            column(3, 
                   selectInput("country", 
                               "Country:", 
                               c("All", 
                                 unique(as.character(pa_per_iso3$iso3_country_name))))
            ),
            column(3, 
                   selectInput("type", 
                               "Type:", 
                               c("All", 
                                 c("polygon", "point")))
            ),
            column(3, 
                   selectInput("iucn_cat", 
                               "IUCN category:", 
                               c("All", 
                                 sort(unique(as.character(pa_per_iso3$iucn_cat)))))
            ),
            column(3, 
                   selectInput("status", 
                               "Status:", 
                               c("All", 
                                 sort(unique(as.character(pa_per_iso3$status)))))
            )
          ),
          # Create a new row for the table.
          fluidRow(
            dataTableOutput(outputId = "table")
          ) 
      ),
      column(4,
        sidebarPanel(
          width = 12,
          selectInput("yaxis", "Y-axis:", 
                      choices=c("Percent", "Count", "Area")),
          hr(),
          helpText("Foo bar."),
          plotOutput("iucncatPlot"),
          
          hr(),
          helpText("Foo bar."),
          plotOutput("statusPlot")
        )
      )
    )
  )  
))
