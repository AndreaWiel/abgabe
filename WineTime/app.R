#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(bootstrap)
library(leaflet)
library(ggplot2)
library(plotly)
library(ggthemes)
library(gghighlight)
library(lubridate)
library(tidyverse)
library(shinydashboard)

# Daten laden

raw <- read_csv("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datens%C3%A4tze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv")
#raw <- read_csv("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datens%C3%A4tze/Wein/Rebfl%C3%A4schen_Anbaugebiete_Jahr_Rebsorte.csv")
#raw <- read_csv("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datens%C3%A4tze/Wein/Erntemenge_Bundesl%C3%A4nder_Jahr_Rebsorte.csv")
#raw <- read_csv("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datens%C3%A4tze/Wein/Erntemenge_Bundesl%C3%A4nder_Jahr_Rebsorte.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
        title ="WineTime",
        theme = "bootstrap.min.css",
        footer = includeHTML("footer.html"),
        fluid = TRUE, 
        collapsible = TRUE,

        # tabPanel 1 - Home
        tabPanel("Home"
        ),
        
        # tabPanel 2 - Weinanbaugebiete
        tabPanel("Weinanbaugebiete"
        ),
        
        # tabPanel 3 - Ernte
        tabPanel("Ernte"
        ),
        
        # tabPanel 4 - Weinproduktion
        tabPanel("Weinproduktion"
        ),
        
        # tabPanel 5 - Weinbestände
        tabPanel("Weinbestände"
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
