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
RF_ABG_Jahr_RS_neu <- RF_ABG_Jahr_RS %>%
                      gather("Jahr", "ha", 3:28)
RF_ABG_Op <- RF_ABG_Jahr_RS_neu$Anbaugebiet %>% unique()
RF_RS_Op <- RF_ABG_Jahr_RS_neu$Rebsorte %>% unique()


E_BL_Jahr_RS_neu <- E_BL_Jahr_RS %>%
  gather("Weinsorte_Ernte", "n", 3:11)


WP_BL_Jahr_RS_neu <- WP_BL_Jahr_RS %>%
                     gather("Weinsorte", "hl", 3:14)
  

WB_BL_Jahr_RS_neu <- WB_BL_Jahr_RS %>%
                     gather("Jahr", "hl", 3:28)
WB_BL_Op <- WB_BL_Jahr_RS_neu$Bundesland %>% unique()
WB_RS_Op <- WB_BL_Jahr_RS_neu$Rebsorte %>% unique()

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
        #  color: #B3056A;")), #9e0657
        
        # tabPanel 1 - Home ----
        tabPanel("Home",
                 includeHTML("home.html")
        ),
        
        # tabPanel 2 - Weinanbaugebiete ----
        navbarMenu("Weinanbaugebiete",
                    tabPanel("Die deutschen Anbaugebiete",
                             includeHTML("Weinanbau.html"),
                             sidebarLayout(
                               sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                            sliderInput("Jahr2.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                            selectInput("Anbaugebiet2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1])
                               ),
                               mainPanel(h4(strong("Die deutschen Anbaugebiete")),
                                  tabsetPanel(
                                      tabPanel("Grafik",
                                                plotOutput('Weinanbaugebiete1.1')
                                      ),
                                      tabPanel("Tabelle",
                                                DT::DTOutput('Weinanbaugebiete1.2')
                                      )
                                  )
                               )
                             )
                    ),
                    tabPanel("Anbaugebiete im Zeitvergleich",
                             includeHTML("Weinanbau.html"),
                             sidebarLayout(
                               sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                            selectInput("Anbaugebiet2.2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1])
                               ),
                               mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                  tabsetPanel(
                                      tabPanel("Grafik",
                                                plotOutput('Weinanbaugebiete2.1')
                                      ),
                                      tabPanel("Tabelle",
                                                DT::DTOutput('Weinanbaugebiete2.2')
                                      )
                                  )
                               )
                             ),
                             sidebarLayout(
                               sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                            selectInput("Anbaugebiet2.2.2", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1]),
                                            selectInput("Anbaugebiet2.2.3", "Wählen Sie ein zweites Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[2]),
                                            selectInput("Rebsorte2.2", "Wählen Sie eine Rebsorte", choices = RF_RS_Op, selected = RF_RS_Op[1]) 
                               ),
                               mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                         tabsetPanel(
                                           tabPanel("Grafik",
                                                    plotOutput('Weinanbaugebiete2.3')
                                           ),
                                           tabPanel("Tabelle",
                                                     DT::DTOutput('Weinanbaugebiete2.4')
                                           )
                                         )
                               )
                             )
                    ),
                    tabPanel("Anbaugebiete im Ländervergleich",
                             includeHTML("Weinanbau.html"),
                             sidebarLayout(
                               sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                            sliderInput("Jahr2.3.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                            selectInput("Rebsorte2.3.1", "Wählen Sie eine Rebsorte", choices = RF_RS_Op, selected = RF_RS_Op[1])
                               ),
                               mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                         tabsetPanel(
                                           tabPanel("Grafik",
                                                    plotOutput('Weinanbaugebiete3.1')
                                           ),
                                           tabPanel("Tabelle",
                                                     DT::DTOutput('Weinanbaugebiete3.2')
                                           )
                                         )
                               )
                             ),
                             sidebarLayout(
                               sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                            sliderInput("Jahr2.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                            sliderInput("Jahr2.3.3", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                            selectInput("Rebsorte2.3.2", "Wählen Sie eine Rebsorte", choices = RF_RS_Op, selected = RF_RS_Op[1]) 
                               ),
                               mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                         tabsetPanel(
                                           tabPanel("Grafik",
                                                    plotOutput('Weinanbaugebiete3.3')
                                           ),
                                           tabPanel("Tabelle",
                                                     DT::DTOutput('Weinanbaugebiete3.4')
                                           )
                                         )
                               )
                             )
                    )
        ),
        
        # tabPanel 3 - Ernte ----
        tabPanel("Weinernte",
                 includeHTML("Weinernte.html"),
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("Jahr3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, sep = ""),
                     selectInput("Bundesland3", "Wählen Sie ein Bundesland:", choices = E_BL_Jahr_RS_neu$Bundesland)
                   ),
                   mainPanel(
                     plotOutput('Weinernte1')
                   )
                 )
),
        
        # tabPanel 4 - Weinproduktion ----
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
                  tabPanel("Weinbestände der Bundesländer",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(h4(strong("Auswhlmöglichkeiten")),
                                sliderInput("Jahr5.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                selectInput("Bundesland5.1", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1])
                             ),
                             mainPanel(h4(strong("Weinbestände der Bundesländer")),
                                tabsetPanel(
                                  tabPanel("Grafik",
                                            plotOutput('Weinbestand1.1')
                                  ),
                                  tabPanel("Tabelle",
                                            DT::DTOutput('Weinbestand1.2')
                                  )
                                )
                             )
                           )
                  ),
                  tabPanel("Weinbestände im Zeitvergleich",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                          selectInput("Bundesland5.2", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1])
                             ),
                             mainPanel(h4(strong("Weinbestände im Zeitvergleich")),
                                tabsetPanel(
                                  tabPanel("Grafik",
                                            plotOutput('Weinbestand2.1')
                                  ),
                                  tabPanel("Tabelle",
                                            DT::DTOutput('Weinbestand2.2')
                                  )
                                )
                             )
                           )
                  ),
                  tabPanel("Weinbestände im Ländervergleich",
                           includeHTML("Weinbestand.html"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("Jahr5.3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                               selectInput("Rebsorte5.3", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1])
                             ),
                             mainPanel(h4(strong("Weinbestände im Ländervergleich")),
                               tabsetPanel(
                                 tabPanel("Grafik",
                                          plotOutput('Weinbestand3.1')
                                 ),
                                 tabPanel("Tabelle",
                                          DT::DTOutput('Weinbestand3.2')
                                 )
                               )
                             )
                           )
                  )
        )
  
)
        


# Define server logic required to draw a histogram
server <- function(input, output) {

      
      # tabPanel 2 - Weinanbaugebiete ----
      output$Weinanbaugebiete1.1 <- renderPlot({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
          filter(Jahr == input$Jahr2.1) %>% 
          ggplot() +
          aes(x = Rebsorte, y = ha) +
          geom_col(position = "dodge") +
          scale_fill_manual(values = c(Weisswein = "#8aa4be", Rotwein = "#9e0657", Insgesamt = "#2c3e50")) +
          labs(
            x = "Rebsorte",
            y = "Anbaufläsche in ha",
          caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
      
      output$Weinanbaugebiete1.2 <- DT::renderDT({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
          filter(Jahr == input$Jahr2.1)
      })
      
      output$Weinanbaugebiete2.1 <- renderPlot({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.2.1) %>%
          ggplot()+
          aes(x = Jahr, y = ha, color = Rebsorte)+
          geom_point()+
          geom_line()+
          labs(
            x = "Jahr",
            y = "Anbaufläsche in ha",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })
      
      output$Weinanbaugebiete2.2 <- DT::renderDT({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.2.1)
      })
      
      output$Weinanbaugebiete2.3 <- renderPlot({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.2.2 | Anbaugebiet == input$Anbaugebiet2.2.3) %>%
          filter(Rebsorte == input$Rebsorte2.2) %>%
          ggplot()+
          aes(x = Jahr, y = ha, color = Anbaugebiet)+
          geom_point()+
          geom_line()+
          labs(
            x = "Jahr",
            y = "Anbaufläsche in ha",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })
      
      output$Weinanbaugebiete2.4 <- DT::renderDT({
        RF_ABG_Jahr_RS_neu %>%
          filter(Anbaugebiet == input$Anbaugebiet2.2.2 | Anbaugebiet == input$Anbaugebiet2.2.3) %>%
          filter(Rebsorte == input$Rebsorte2.2)
      })
      
      output$Weinanbaugebiete3.1 <- renderPlot({
        RF_ABG_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte2.3.1) %>%
          filter(Jahr == input$Jahr2.3.1) %>%
          ggplot()+
          aes(x = Anbaugebiet, y = ha)+
          geom_col(position = "dodge")+
          labs(
            x = "Anbaugebiet",
            y = "Anbaufläsche in ha",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))
      })
      
      output$Weinanbaugebiete3.2 <- DT::renderDT({
        RF_ABG_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte2.3.1) %>%
          filter(Jahr == input$Jahr2.3.1)
      })
      
      output$Weinanbaugebiete3.3 <- renderPlot({
        RF_ABG_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte2.3.2) %>%
          filter(Jahr == input$Jahr2.3.2 | Jahr == input$Jahr2.3.3) %>%
          ggplot()+
          aes(x = Anbaugebiet, y = ha, color = Jahr)+
          geom_col(position = "dodge")+
          labs(
            x = "Anbaugebiet",
            y = "Anbaufläsche in ha",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))
      })
      
      output$Weinanbaugebiete3.4 <- DT::renderDT({
        RF_ABG_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte2.3.2) %>%
          filter(Jahr == input$Jahr2.3.2 | Jahr == input$Jahr2.3.3)
      })
      
      
      
      
      # tabPanel 3 - Weinernte ----
      output$Weinernte1 <- renderPlot({
        E_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland3) %>%
          filter(Jahr == input$Jahr3) %>%
          ggplot(aes(x = "Weinsorte_Ernte", y = "n")) +
          geom_col(position = "dodge") +
          labs(
            x = "Erntesorte",
            y = "Erntemenge in hl & ha",
            caption = "Quelle & Copyright: Statistisches Bundesamt")})
  
      
      
      
      # tabPanel 4 - Weinproduktion ----
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
      output$Weinbestand1.1 <- renderPlot({
        WB_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland5.1) %>%
          filter(Jahr == input$Jahr5.1) %>% 
          ggplot() +
          aes(x = Rebsorte, y = hl) +
          geom_col(position = "dodge") +
          scale_fill_manual(values = c(Weisswein = "#8aa4be", Rotwein = "#9e0657", Insgesamt = "#2c3e50")) +
          labs(
            x = "Rebsorte",
            y = "Weinbestand in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")
      })
      
      output$Weinbestand1.2 <- DT::renderDT({
        WB_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland5.1) %>%
          filter(Jahr == input$Jahr5.1)
      })
      
      output$Weinbestand2.1 <- renderPlot({
        WB_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland5.2) %>%
          ggplot()+
          aes(x = Jahr, y = hl, color = Rebsorte)+
          geom_point()+
          geom_line()+
          labs(
            x = "Jahr",
            y = "Weinbestand in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })
      
      output$Weinbestand2.2 <- DT::renderDT({
        WB_BL_Jahr_RS_neu %>%
          filter(Bundesland == input$Bundesland5.2)
      })
      
      output$Weinbestand3.1 <- renderPlot({
        WB_BL_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte5.3) %>%
          filter(Jahr == input$Jahr5.3) %>%
          ggplot()+
          aes(x = Bundesland, y = hl)+
          geom_col(position = "dodge")+
          labs(
            x = "Bundesland",
            y = "Weinbestand in hl",
            caption = "Quelle & Copyright: Statistisches Bundesamt")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))
      })
      
      output$Weinbestand3.2 <- DT::renderDT({
        WB_BL_Jahr_RS_neu %>%
          filter(Rebsorte == input$Rebsorte5.3) %>%
          filter(Jahr == input$Jahr5.3)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
