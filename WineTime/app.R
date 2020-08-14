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

# Daten einlesen ----
E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv", na="NA", check.names = FALSE)
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv", na="NA", check.names = FALSE)
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
                     gather("Jahr", "n", 3:28) #%>%
                     #spread(Rebsorte, n)

WP_BL_Jahr_RS_neu <- WP_BL_Jahr_RS %>%
                     gather("Weinsorte", "n", 3:14) %>%
                     spread(Jahr, n)



opt_bundeslaender <- WB_BL_Jahr_RS_neu$Bundesland %>% unique()

# Define UI ----
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
        
        # tabPanel 1 - Home ----
        tabPanel("Home",
                 includeHTML("home.html")
        ),
        
        # tabPanel 2 - Weinanbaugebiete ----
        tabPanel("Weinanbaugebiete",
                 includeHTML("Weinanbau.html"),
                 leaflet::leafletOutput('Map', width = '70%', height = '70%')
        ),
        
        # tabPanel 3 - Ernte ----
        tabPanel("Weinernte",
                 includeHTML("Weinernte.html"),
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("Jahr3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                     selectInput("Bundesland3", "Wählen Sie ein Bundesland:", choices = E_BL_Jahr_RS$Bundesland)
                   ),
                   mainPanel(
                     plotOutput('Weinernte1')
                   )
                 )
),
        
        # tabPanel 4 - Weinproduktion
        navbarMenu("Weinproduktion",
                 tabPanel("Weinproduktion der Bundesländer",
                          includeHTML("Weinproduktion.html"),
                          selectInput("Weinsorte4", "Wählen Sie eine Weinsorte:", choices = WP_BL_Jahr_RS_neu$Weinsorte)
                 ),
                 tabPanel("Weinproduktion im Zeitvergleich",
                          includeHTML("Weinproduktion.html"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Bundesland4", "Wählen Sie ein Bundesland:", choices = WP_BL_Jahr_RS_neu$Bundesland),
                              selectInput("Weinsorte4.1", "Wählen Sie eine Weinsorte:", choices = WP_BL_Jahr_RS_neu$Weinsorte),
                              selectInput("Weinsorte4.2", "Wählen Sie eine zweite Weinsorte:", choices = WP_BL_Jahr_RS_neu$Weinsorte)
                            ),
                            mainPanel(
                              plotOutput('Weinproduktion1'),
                              plotOutput('Weinproduktion2'),
                              plotOutput('Weinproduktion3')
                            )
                          )
                 )
        ),
        
        # tabPanel 5 - Weinbestände ----
        navbarMenu("Weinbestände",
                  tabPanel("Weinbeständen der Bundesländer",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                                sliderInput("Jahr5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                selectInput("Bundesland5", "Wählen Sie ein Bundesland:", choices = opt_bundeslaender,
                                            selected = opt_bundeslaender[1])
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
                               sliderInput("Jahr5x", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                               selectInput("Bundesland5x", "Wählen Sie ein Bundesland:", choices = WB_BL_Jahr_RS_neu$Bundesland)
                             ),
                             mainPanel(
                               plotOutput('Weinbestand1x')
                             )
                           )
                  ),
                  tabPanel("Weinbestände im Ländervergleich",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("Jahr5xx", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010),
                               selectInput("Bundesland5xx", "Wählen Sie ein Bundesland:", choices = WB_BL_Jahr_RS_neu$Bundesland)
                             ),
                             mainPanel(
                               plotOutput('Weinbestand1xx')
                             )
                           )
                  )
        )
  
)
        


# Define server logic required to draw a histogram
server <- function(input, output) {

      
      # tabPanel 2 - Weinanbaugebiete
      output$Map <- leaflet::renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView( -98.58, 39.82, zoom = 5)
      })
      
      # tabPanel 2 - Weinernte
      output$Weinernte1 <- renderPlot({
        E_BL_Jahr_RS %>%
          filter(Bundesland == input$Bundesland3)
        filter(Jahr == input$Jahr3) %>%
          ggplot(aes(x = "", color = Erntemenge_an_Weissmost)) +
          geom_line() +
          labs(
            x = "Jahr",
            y = "Erntemenge",
            caption = "Quelle & Copyright: Statistisches Bundesamt"
          )})
  
      # tabPanel 4 - Weinproduktion
      output$Weinproduktion1 <- renderPlot({
        WP_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland4) %>%
          filter(Weinsorte == input$Weinsorte1) %>%
          ggplot(aes(x = "Jahr"))+
          geom_histogram()+
          labs(
            x = "Jahr",
            y = "Weinproduktion in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
      
      output$Weinproduktion2 <- renderPlot({
        WP_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland4) %>%
          filter(Weinsorte == input$Weinsorte2) %>%
          ggplot(aes(x = "Jahr"))+
          geom_histogram()+
          labs(
            x = "Jahr",
            y = "Weinproduktion in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
      
      output$Weinproduktion3 <-renderPlot({
        WP_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland4) %>%
          filter(Weinsorte == input$Weinsorte1|input$Weinsorte2) %>%
          ggplot(aes(x = "Jahr", color = "Weinsorte"))+
          geom_histogram()+
          labs(
            x = "Jahr",
            y = "Weinproduktion in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
      
      
      # tabPanel 5 - Weinbestände ----
      output$Weinbestand1 <- renderPlot({
        
        #print(input$Bundesland5)
        
        temp <- WB_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland5) %>%
          filter(Jahr == input$Jahr5) 
        #print(nrow(temp))
        temp %>% 
          ggplot() +
          #aes(x = "", color = Rebsorte) +
          aes(x = Rebsorte, y = n) +
          geom_col() +
          labs(
            x = "Rebsorte",
            y = "Weinbestand in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
