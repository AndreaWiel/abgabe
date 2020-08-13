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
library(dplyr)

# Daten laden


E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv", na="NA", check.names = FALSE)
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv", na="NA", check.names = FALSE)
WB_BL_Jahr_RS2 <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestand.csv", na="NA", check.names = FALSE)
WP_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv", na="NA")
Frost <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Frosttage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Sommert <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sommertage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Sonne <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sommertage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Regen <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittsniederschlag_Bundesland_Jahr.csv", na="NA")
TempDurch <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittstemperatur_Bundeslaender_Jahr_2.csv", na="NA")

selectable <- function(x)
{
    is.numeric(x) | is.Date(x)
}


our_variables <- names(E_BL_Jahr_RS %>% select_if(selectable))

BL <- E_BL_Jahr_RS$Bundesland %>% unique() %>% sort()
JahrWein <- E_BL_Jahr_RS$Jahr %>% unique() %>% sort()
IEM <- E_BL_Jahr_RS$Erntemenge_an_Weinmost %>% unique() %>% sort()
RFEW <- E_BL_Jahr_RS$Rebfläche_im_Ertrag_Weißmost %>% unique() %>% sort()
RFR <- E_BL_Jahr_RS$Erntemenge_an_Rotmost %>% unique() %>% sort()
REH <- E_BL_Jahr_RS$Rotmostertrag_je_Hektar %>% unique() %>% sort()
RFER <- E_BL_Jahr_RS$Rebfläche_im_Ertrag_Rotmost %>% unique() %>% sort()
IEMWM <- E_BL_Jahr_RS$Insgesamte_Erntemenge_an_Weinmost %>% unique() %>% sort()
IWEH <- E_BL_Jahr_RS$Insgesamter_Weinmostertrag_je_Hektar %>% unique() %>% sort()
IRFE <- E_BL_Jahr_RS$Insgesamte_Rebfläche_im_Ertrag %>% unique() %>% sort()
IWEH <- E_BL_Jahr_RS$Insgesamter_Weinmostertrag_je_Hektar %>% unique() %>% sort()

#Daten drehen
WB_BL_Jahr_RS_neu <- WB_BL_Jahr_RS %>%
                      slice(1:28,1:48) %>%
                      gather("Jahr", "n", 3:28) %>%
                      spread(Rebsorte, n)


# Define UI for application that draws a histogram
ui <- navbarPage(title = "WineTime",
                 theme = "bootstrap.css",
                 footer = includeHTML("footer.html"),
                 fluid = TRUE, 
                 collapsible = TRUE,
  
  
 # tags$head(
#    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Dancing+Script:wght@700&display=swap"))
#  ),
  
 # headerPanel(
  #  h1("WineTime",
     #   style = "font-family: 'Dancing Script', cursive;
      #    font-weight: 900; line-height: 3.2; 
        #  color: #B3056A;")),
        
        # tabPanel 1 - Home
        tabPanel("Home",
                 includeHTML("home.html"),
        ),
        
        # tabPanel 2 - Weinanbaugebiete
        tabPanel("Weinanbaugebiete",
                 includeHTML("Weinanbau.html"),
                 plotOutput('Map')
        ),
        
        # tabPanel 3 - Ernte
        tabPanel("Weinernte",
                 includeHTML("Weinernte.html")
        ),
        
        # tabPanel 4 - Weinproduktion
        tabPanel("Weinproduktion",
                 includeHTML("Weinproduktion.html")
        ),
        
        # tabPanel 5 - Weinbestände
        navbarMenu("Weinbestände",
                  tabPanel("Weinbeständen der Bundesländer",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                                sliderInput("Jahr5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                                selectInput("Bundesland5", "Wählen Sie ein Bundesland:", choices = WB_BL_Jahr_RS$Bundesland)
                             ),
                             mainPanel(
                                plotOutput('Weinbestand1')
                             )
                           )
                  ),
                  tabPanel("Weinbestände im Zeitvergleich",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("Jahr5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                               selectInput("Bundesland5", "Wählen Sie ein Bundesland:", choices = WB_BL_Jahr_RS$Bundesland)
                             ),
                             mainPanel(
                               plotOutput('Weinbestand1')
                             )
                           )
                  ),
                  tabPanel("Weinbestönde im Ländervergleich",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("Jahr5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                               selectInput("Bundesland5", "Wählen Sie ein Bundesland:", choices = WB_BL_Jahr_RS$Bundesland)
                             ),
                             mainPanel(
                               plotOutput('Weinbestand1')
                             )
                           )
                  )
        )
  
  
  
     #   sliderInput(inputId = "num", 
               #     label = "Choose a number", 
               #     value = 25, min = 1, max = 100),
      #  plotOutput("hist"))
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
)
        


# Define server logic required to draw a histogram
server <- function(input, output) {

      
      # tabPanel 2 - Weinanbaugebiete
      output$Map <- leaflet::renderLeaflet({
        
      })
  
      # tabPanel 5 - Weinbestände
      output$Weinbestand1 <- renderPlot({
        WB_BL_Jahr_RS %>%
          filter(Bundesland == input$Bundesland5) %>%
          filter(Jahr == input&Jahr5) %>%
          ggplot(aes(x = "", color = Rebsorte)) +
          geom_bar() +
          labs(
            x = "Rebsorte",
            y = "Weinbestand in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
