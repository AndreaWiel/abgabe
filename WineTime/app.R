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

<<<<<<< HEAD
#E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
#RF_ABG_Jahr_RS <- read.csv2("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv")
#WB_BL_Jahr_RS <- read.csv2("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv")
#WP_BL_Jahr_RS <- read.csv2("https://github.com/AndreaWiel/abgabe/blob/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv")
=======
E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv")
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv")
WP_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv")
>>>>>>> 1b7d96e5ecac7935010083c124d54982f75b4bd0

#selectable <- function(x)
#{
#    is.numeric(x) | is.Date(x)
#}
<<<<<<< HEAD

#our_variables <- names(raw %>% select_if(selectable))
=======

#our_variables <- names(raw %>% select_if(selectable))

#lks <- raw$Landkreis %>% unique() %>% sort()
#ages <- raw$Altersgruppe %>% unique() %>% sort()
>>>>>>> 1b7d96e5ecac7935010083c124d54982f75b4bd0

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
