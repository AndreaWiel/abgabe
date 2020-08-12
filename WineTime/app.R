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


E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv")
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv")
WP_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv")


selectable <- function(x)
{
    is.numeric(x) | is.Date(x)
}


our_variables <- names(E_BL_Jahr_RS %>% select_if(selectable))

BL <- E_BL_Jahr_RS$Bundesland %>% unique() %>% sort()
Jahr <- E_BL_Jahr_RS$Jahr %>% unique() %>% sort()
IEM <- E_BL_Jahr_RS$Erntemenge_an_Weinmost %>% unique() %>% sort()
RFEW <- E_BL_Jahr_RS$Rebfläche_im_Ertrag_Weißmost %>% unique() %>% sort()
RFR <- E_BL_Jahr_RS$Erntemenge_an_Rotmost %>% unique() %>% sort()
REH <- E_BL_Jahr_RS$Rotmostertrag_je_Hektar %>% unique() %>% sort()
RFER <- E_BL_Jahr_RS$Rebfläche_im_Ertrag_Rotmost %>% unique() %>% sort()
IEMWM <- E_BL_Jahr_RS$Insgesamte_Erntemenge_an_Weinmost %>% unique() %>% sort()
IWEH <- E_BL_Jahr_RS$Insgesamter_Weinmostertrag_je_Hektar %>% unique() %>% sort()
IRFE <- E_BL_Jahr_RS$Insgesamte_Rebfläche_im_Ertrag %>% unique() %>% sort()
IWEH <- E_BL_Jahr_RS$Insgesamter_Weinmostertrag_je_Hektar %>% unique() %>% sort()


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
        ),
        sliderInput(inputId = "num", 
                    label = "Choose a number", 
                    value = 25, min = 1, max = 100),
        plotOutput("hist"))
       # WeinanbaugebieteBody(
        # UI Tabs ----
       # tabItems(
            # UI Tabs dashboard ----
          #  tabItem(tabName = "Weinanbaugebiete",
                  #  fluidRow(
                       # box( width = 8,
                           #  plotOutput("demovariablen"),
                      #      # selectInput("varselect", "Variable auswählen", our_variables)
                       # ),
                       # box( width = 8,
                            # title = "Fälle pro Tag",
                            # plotlyOutput("plot1"),
                            # plotOutput("plotDetail"),
                            # plotOutput("plotSumme", 
                                      # hover = hoverOpts(id = "plot_hover", delayType = "debounce"),
                                       # brush = brushOpts(id = "plot_brush", direction = "x")),
                            # plotOutput("unser_brush")
                      #  ),
                       # box(
                          #  width = 4, 
                          #  title = "Controls",
                           # selectInput("lk_selector", "Landkreis auswählen", choices = lks),
                           # selectInput("age_selector", "Altersgruppe auswählen", choices = ages),
                           # checkboxInput("facet_plot_yes_no", "Plot facetieren?", value = FALSE),
                           # selectInput("theme_selector", "Theme auswählen", choices = c("bw", "economist", "fivethirtyeight")),
                           # actionButton("dashboard_next", "Weiter")
                       # ),
                        
                        #box(width = 8,
                           # title = "Empty",
                           # textOutput("error"),
                           # verbatimTextOutput("debug")
                       # )
                  #  )
                    
           # ),
            # UI Tabs widgets ----
            #tabItem(tabName = "Über den Datensatz", 
                  #  fluidRow(
                    #    box(width = 12,
                      #     title = "Empty2",
                       #    h1("Überschrift"),
                      #     h2("Überschrift 2"),
                       #     h3("Überschrift 3"),
                       #     h4("Überschrift 4"),
                         #   p("Hallo das ist ein Test"),
                        #    br(), br(), 
                        #    p("Zweiter Paragraph"),
                        #    a(href = "www.google.com", "Hallo"),
                         #   strong(p("Test")),
                         #   hr()
                            
                            
                            
                      #  )
                   # )
           # )
            
      #  )
#)
#)
        


# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
