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
library(ggplot2)
library(plotly)
library(ggthemes)
library(gghighlight)
library(lubridate)
library(tidyverse)
library(shinydashboard)
library(dplyr)

# Daten einlesen ----
E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na="NA")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv", na="NA", check.names = FALSE)
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv", na="NA", check.names = FALSE)
WP_BL_Jahr_WK <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv", na="NA")
Frosttage <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Frosttage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Sommertage <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sommertage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Sonnenstunden <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sommertage_Bundesl%C3%A4nder_Jahr.csv", na="NA")
Regen <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittsniederschlag_Bundesland_Jahr.csv", na="NA")
TempDurch <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittstemperatur_Bundeslaender_Jahr_2.csv", na="NA")

selectable <- function(x)
{
  is.numeric(x) | is.Date(x)
}


our_variables <- names(E_BL_Jahr_RS %>% select_if(selectable))




# Daten aufbereiten ----

## Daten Rebfäche
RF_ABG_Jahr_RS_neu <- RF_ABG_Jahr_RS %>%
  gather("Jahr", "ha", 3:28)

RF_ABG_Op <- RF_ABG_Jahr_RS_neu$Anbaugebiet %>% unique()
RF_RS_Op <- RF_ABG_Jahr_RS_neu$Rebsorte %>% unique()


## Daten Ernte
E_BL_Jahr_RS_EM <- E_BL_Jahr_RS %>%
  select(1:3, 6, 9) %>%
  rename(c("Weißmost" = "Erntemenge_an_Weissmost", "Rotmost" = "Erntemenge_an_Rotmost", "Weinmost insgesamt" = "Insgesamte_Erntemenge_an_Weinmost")) %>%
  gather("Mostsorte", "Erntemenge in hl", 3:5)

E_BL_Jahr_RS_EE <- E_BL_Jahr_RS %>%
  select(1:2, 4, 7, 10) %>%
  rename(c("Weißmost" = "Weissmostertrag_je_Hektar", "Rotmost" = "Rotmostertrag_je_Hektar", "Weinmost insgesamt" = "Insgesamter_Weinmostertrag_je_Hektar")) %>%
  gather("Mostsorte", "Weinmostertrag je Hektar in hl", 3:5)

E_BL_Jahr_RS_RE <- E_BL_Jahr_RS %>%
  select(1:2, 5, 8, 11) %>%
  rename(c("Weißmost" = "Rebflaeche_im_Ertrag_Weissmost", "Rotmost" = "Rebflaeche_im_Ertrag_Rotmost", "Weinmost insgesamt" = "Insgesamte_Rebflaeche_im_Ertrag")) %>%
  gather("Mostsorte", "Rebfläche im Ertrag in ha", 3:5)

E_BL_Jahr_RS_EM_EE <- left_join(E_BL_Jahr_RS_EM, E_BL_Jahr_RS_EE, by = c("Bundesland", "Jahr", "Mostsorte"))
E_BL_Jahr_RS_neu <- left_join(E_BL_Jahr_RS_EM_EE, E_BL_Jahr_RS_RE, by = c("Bundesland", "Jahr", "Mostsorte")) %>%
  gather("Messparameter", "Wert", 4:6)
  
    #E_BL_Jahr_RS_neu <- E_BL_Jahr_RS %>%
    #  gather("Ernte_und_Ertrag", "hl_oder_ha", 3:11)

E_BL_Op <- E_BL_Jahr_RS_neu$Bundesland %>% unique()
E_MS_Op <- E_BL_Jahr_RS_neu$Mostsorte %>% unique()
E_MP_Op <- E_BL_Jahr_RS_neu$Messparameter %>% unique()


## Daten Weinproduktion
WP_BL_Jahr_WK_neu <- WP_BL_Jahr_WK %>%
  gather("Weinkategorie", "hl", 3:14)

WP_BL_Op <- WP_BL_Jahr_WK_neu$Bundesland %>% unique()
WP_WK_Op <- WP_BL_Jahr_WK_neu$Weinkategorie %>% unique() 


## Daten Weinbestand
WB_BL_Jahr_RS_neu <- WB_BL_Jahr_RS %>%
  gather("Jahr", "hl", 3:28)

WB_BL_Op <- WB_BL_Jahr_RS_neu$Bundesland %>% unique()
WB_RS_Op <- WB_BL_Jahr_RS_neu$Rebsorte %>% unique()


## Daten Frosttage
Frosttage_neu <- Frosttage %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Wüerttemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen")) %>%
  gather("Bundesland", "Frosttage", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Regenmenge
Regen_neu <- Regen %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Wüerttemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen")) %>%
  gather("Bundesland", "Regenmenge in mm (1mm = 1l/m²)", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Sonnenstunden
Sonnenstunden_neu <- Sonnenstunden %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Wüerttemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen")) %>%
  gather("Bundesland", "Sonnenstunden", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Sommertage
Sommertage_neu <- Sommertage %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Wüerttemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen")) %>%
  gather("Bundesland", "Sommertage", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Temperatur
TempDurch_neu <- TempDurch %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Wüerttemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen")) %>%
  gather("Bundesland", "Temperaturdurchschnitt in °C", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Wetter insgesamt

Wetter_ST_FT <- left_join(Sommertage_neu, Frosttage_neu, by = c("Jahr", "Bundesland"))
Wetter_ST_FT_SST <- left_join(Wetter_ST_FT, Sonnenstunden_neu, by = c("Jahr", "Bundesland"))
Wetter_ST_FT_SST_R <- left_join(Wetter_ST_FT_SST, Regen_neu, by = c("Jahr", "Bundesland"))
Wetter_gesamt <- left_join(Wetter_ST_FT_SST_R, TempDurch_neu, by = c("Jahr", "Bundesland")) %>%
  gather("Wetterphänomen", "Wert", 3:7)

Wetter_BL_Op <- Wetter_gesamt$Bundesland %>% unique()
Wetter_WP_Op <- Wetter_gesamt$Wetterphänomen %>% unique()


#Wetter_final <- Wetter_zusam %>%
 # gather("Wetter", "Anzahl_Tage_u_Temp", 3:7)

#Wetter_Ernte <- merge(Wetter_final, E_BL_Jahr_RS_neu)


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
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr2.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Anbaugebiet2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1])
                                       ),
                                       mainPanel(h4(strong("Die deutschen Anbaugebiete")),
                                                 textOutput('Wahl2.1'),
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
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Anbaugebiet2.2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1])
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                                 textOutput('Wahl2.2.1'),
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
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Anbaugebiet2.2.2", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1]),
                                                    selectInput("Anbaugebiet2.2.3", "Wählen Sie ein weiteres Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[2]),
                                                    selectInput("Rebsorte2.2", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                                 textOutput('Wahl2.2.2'),
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
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr2.3.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte2.3.1", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                                 textOutput('Wahl2.3.1'),
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
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr2.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr2.3.3", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte2.3.2", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                                 textOutput('Wahl2.3.2'),
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
                 # navbarMenu("Weinernte & Wetter",
                 #            tabPanel("Mehr hierzu",
                 #                     includeHTML("Weinernte.html"),
                 #                     sidebarLayout(
                 #                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                 #                                    sliderInput("Jahr3.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                 #                                    selectInput("Bundesland3.1", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1])),
                 #                       mainPanel(h4(strong("Weinernte nach Bundesländern")),
                 #                                 tabsetPanel(
                 #                                   tabPanel("Grafik",
                 #                                            plotOutput('Weinernte1.1')
                 #                                   ),
                 #                                   tabPanel("Tabelle",
                 #                                            DT::DTOutput('Weinernte1.2')
                 #                                   )
                 #                                 )
                 #                       )
                 #                       
                 #                     ),
                 #                     
                 #                     sidebarLayout(
                 #                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                 #                                    sliderInput("Jahr3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                 #                                    sliderInput("Jahr3.3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 1995, step = 1, sep = ""),
                 #                                    selectInput("Bundesland3.2", "Wählen Sie ein Bundesland:", choices = Wetter_Ernte$Bundesland, selected = Wetter_Ernte$Bundesland[1])
                 #                       ),
                 #                       mainPanel(h4(strong("Wetter über die Jahre")),
                 #                                 tabsetPanel(
                 #                                   tabPanel("Grafik",
                 #                                            plotOutput('Wetter1.1')
                 #                                   ),
                 #                                   tabPanel("Tabelle",
                 #                                            DT::DTOutput('Wetter1.2')
                 #                                   ))
                 #                                       )
                 #                                          ))
                 #                                              ), 
                 # 
                 navbarMenu("Weinernte",
                            tabPanel("Weinernte & Wetter der Bundesländer",
                                     includeHTML("Weinernte.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.1.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland3.1.1", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Messparameter3.1", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinernte der Bundesländer")),
                                                 textOutput('Wahl3.1.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinernte1.1', height = 600)
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinernte1.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.1.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland3.1.2", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    h6("*Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetterdaten der Bundesländer")),
                                                 textOutput('Wahl3.1.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Wetter1.1', height = 600)
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Wetter1.2')
                                                   )
                                                 )
                                       )
                                     )
                            ),
                            tabPanel("Weinernte & Wetter im Zeitvergleich",
                                     includeHTML("Weinernte.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.1", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Messparameter3.2.1", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinernte im Zeitvergleich")),
                                                 textOutput('Wahl3.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinernte2.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinernte2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.2", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    h6("*Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetter im Zeitvergleich")),
                                                 textOutput('Wahl3.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Wetter2.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Wetter2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.3", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Bundesland3.2.4", "Wählen Sie ein weiteres Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Messparameter3.2.2", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    selectInput("Mostsorte3.2", "Wählen Sie eine Mostsorte", choices = E_MS_Op, selected = E_MS_Op)
                                       ),
                                       mainPanel(h4(strong("Weinernte im Zeitvergleich")),
                                                 textOutput('Wahl3.2.3'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinernte2.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinernte2.4')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.5", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    selectInput("Bundesland3.2.6", "Wählen Sie ein weiteres Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[2]),
                                                    selectInput("Wetterphänomen3.2", "Wählen Sie eine Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1]),
                                                    h6("*Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetter im Zeitvergleich")),
                                                 textOutput('Wahl3.2.4'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Wetter2.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Wetter2.4')
                                                   )
                                                 )
                                       )
                                     )
                                     
                            ),
                            tabPanel("Weinernte & Wetter im Ländervergleich",
                                     includeHTML("Weinernte.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Messparameter3.3.1", "Wählen Sie ein Messparameter:", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    selectInput("Mostsorte3.3.1", "Wählen Sie eine Mostsorte:", choices = E_MS_Op, selected = E_MS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinernte im Ländervergleich")),
                                                 textOutput('Wahl3.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinernte3.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinernte3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    selectInput("Wetterphänomen3.3.1", "Wählen Sie eine Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Wetter im Ländervergleich")),
                                                 textOutput('Wahl3.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Wetter3.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Wetter3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    sliderInput("Jahr3.3.4", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Messparameter3.3.2", "Wählen Sie ein Messparameter:", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    selectInput("Mostsorte3.3.2", "Wählen Sie eine Mostsorte:", choices = E_MS_Op, selected = E_MS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinernte im Ländervergleich")),
                                                 textOutput('Wahl3.3.3'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinernte3.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinernte3.4')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr3.3.6", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Wetterphänomen3.3.2", "Wählen Sie ein Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Wetter im Ländervergleich")),
                                                 textOutput('Wahl3.3.4'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Wetter3.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Wetter3.4')
                                                   )
                                                 )
                                       )
                                     )
                            )
                            
                 ),
                 
                 # tabPanel 4 - Weinproduktion ----
                 navbarMenu("Weinproduktion",
                            tabPanel("Weinproduktion der Bundesländer",
                                     includeHTML("Weinproduktion.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr4.1", "Wählen Sie ein Jahr:", min = 2010, max = 2018, value = 2012, step = 1, sep = ""),
                                                    selectInput("Bundesland4.1", "Wählen Sie ein Bundesland:", choices = WP_BL_Op, selected = WP_BL_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände der Bundesländer")),
                                                 textOutput('Wahl4.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinproduktion1.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinproduktion1.2')
                                                   )
                                                 )
                                       )
                                     )
                            ),
                            
                            tabPanel("Weinproduktion im Zeitvergleich",
                                     includeHTML("Weinproduktion.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland4.2.1", "Wählen Sie ein Bundesland:", choices = WP_BL_Op, selected = WP_BL_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Zeitvergleich")),
                                                 textOutput('Wahl4.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinproduktion2.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinproduktion2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland4.2.2", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    selectInput("Bundesland4.2.3", "Wählen Sie ein weiteres Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[2]),
                                                    selectInput("Weinkategorie4.2", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Zeitvergleich")),
                                                 textOutput('Wahl4.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinproduktion2.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinproduktion2.4')
                                                   )
                                                 )
                                       )
                                     )
                            ),
                            tabPanel("Weinproduktion im Ländervergleich",
                                     includeHTML("Weinproduktion.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr4.3.1", "Wählen Sie ein Jahr:", min = 2010, max = 2018, value = 2012, step = 1, sep = ""),
                                                    selectInput("Weinkategorie4.3.1", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Ländervergleich")),
                                                 textOutput('Wahl4.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinproduktion3.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinproduktion3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr4.3.2", "Wählen Sie ein Jahr:", min = 2010, max = 2018, value = 2012, step = 1, sep = ""),
                                                    sliderInput("Jahr4.3.3", "Wählen Sie ein weiteres Jahr:", min = 2010, max = 2018, value = 2015, step = 1, sep = ""),
                                                    selectInput("Weinkategorie4.3.2", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Ländervergleich")),
                                                 textOutput('Wahl4.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinproduktion3.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinproduktion3.4')
                                                   )
                                                 )
                                       )
                                     )
                            )
                            
                 ),
                 
                 # tabPanel 5 - Weinbestände ----
                 navbarMenu("Weinbestände",
                            tabPanel("Weinbestände der Bundesländer",
                                     includeHTML("Weinbestand.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr5.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland5.1", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände der Bundesländer")),
                                                 textOutput('Wahl5.1'),
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
                                                    selectInput("Bundesland5.2.1", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Zeitvergleich")),
                                                 textOutput('Wahl5.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinbestand2.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinbestand2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland5.2.2", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    selectInput("Bundesland5.2.3", "Wählen Sie ein weiteres Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[2]),
                                                    selectInput("Rebsorte5.2", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Zeitvergleich")),
                                                 textOutput('Wahl5.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinbestand2.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinbestand2.4')
                                                   )
                                                 )
                                       )
                                     )
                            ),
                            tabPanel("Weinbestände im Ländervergleich",
                                     includeHTML("Weinbestand.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr5.3.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte5.3.1", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Ländervergleich")),
                                                 textOutput('Wahl5.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinbestand3.1')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinbestand3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr5.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr5.3.3", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte5.3.2", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Ländervergleich")),
                                                 textOutput('Wahl5.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik",
                                                            plotOutput('Weinbestand3.3')
                                                   ),
                                                   tabPanel("Tabelle",
                                                            DT::DTOutput('Weinbestand3.4')
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
  output$Wahl2.1 <- renderText({
    paste("Weinanbaufläche (in ha) nach Rebsorten für das Anbaugebiet", input$Anbaugebiet2.1, "im Jahr", input$Jahr2.1, ".")
  })
  
  output$Weinanbaugebiete1.1 <- renderPlot(width = "auto", height = 600, {
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
      filter(Jahr == input$Jahr2.1) %>%
      ggplot() +
      aes(x = Rebsorte, y = ha) +
      geom_col(position = "dodge", fill = c("#8aa4be", "#9e0657", "#2c3e50")) +
      geom_label(aes(label=ha)) + 
      ylim(0, 110000) +
      labs(
        x = "Rebsorte",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
  })
  
  output$Weinanbaugebiete1.2 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
      filter(Jahr == input$Jahr2.1)
  })
  
  output$Wahl2.2.1 <- renderText({
    paste("Weinanbaufläche (in ha) nach Rebsorten für das Anbaugebiet", input$Anbaugebiet2.2.1, "zwischen 1993 und 2018.")
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
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinanbaugebiete2.2 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.1)
  })
  
  output$Wahl2.2.2 <- renderText({
    paste("Weinanbaufläche (in ha) der Rebsorte", input$Rebsorte2.2, "zwischen 1993 und 2018 im Vergleich der Anbaugebiete", input$Anbaugebiet2.2.2, "und", input$Anbaugebiet2.2.3, ".")
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
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinanbaugebiete2.4 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.2 | Anbaugebiet == input$Anbaugebiet2.2.3) %>%
      filter(Rebsorte == input$Rebsorte2.2)
  })
  
  output$Wahl2.3.1 <- renderText({
    paste("Weinanbaufläche (in ha) der deutschen Anbaugebiete für die Rebsorte", input$Rebsorte2.3.2, "im Jahr", input$Jahr2.3.1, ".")
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
        y = "Anbaufläche in ha",
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
  
  output$Wahl2.3.2 <- renderText({
    paste("Weinanbaufläche (in ha) der deutschen Anbaugebiete für die Rebsorte", input$Rebsorte2.3.2, "im Vergleich der Jahre", input$Jahr2.3.2, "und", input$Jahr2.3.3, ".")
  })
  
  output$Weinanbaugebiete3.3 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte2.3.2) %>%
      filter(Jahr == input$Jahr2.3.2 | Jahr == input$Jahr2.3.3) %>%
      ggplot()+
      aes(x = Anbaugebiet, y = ha, fill = Jahr)+
      geom_col(position = "dodge")+
      labs(
        x = "Anbaugebiet",
        y = "Anbaufläche in ha",
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
  # output$Weinernte1.1 <- renderPlot({
  #   E_BL_Jahr_RS_neu %>%
  #     filter(Bundesland == input$Bundesland3.1) %>%
  #     filter(Jahr == input$Jahr3.1) %>%
  #     ggplot() +
  #     aes(x = Ernte_und_Ertrag, y = hl_oder_ha) +
  #     geom_col(position = "dodge") +
  #     #coord_cartesian(ylim = c(0, 10000000), expand = TRUE) +
  #     #scale_y_discrete(limits = factor(0, 1000000)) +
  #     labs(
  #       x = "Ernte & Ertrag",
  #       y = "Erntemenge in hl & ha",
  #       caption = "Quelle & Copyright: Statistisches Bundesamt") +
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # })
  # 
  # output$Weinernte1.2 <- DT::renderDT({
  #   E_BL_Jahr_RS_neu %>%
  #     filter(Bundesland == input$Bundesland3.1) %>%
  #     filter(Jahr == input$Jahr3.1)
  # })
  # 
  # output$Wetter <- renderPlot({
  #   Wetter_final %>%
  #     filter(Bundesland == input$Bundesland3.2) %>%
  #     filter(Jahr == input$Jahr3.2 | Jahr == input$Jahr3.3) %>%
  #     ggplot() +
  #     aes(x = Wetter, y = Anzahl_Tage_u_Temp) +
  #     geom_col() +
  #     #facet_grid(Bundesland ~ ., labeller = as_labeller(Bundesländer))+
  #     #scale_colour_discrete(name="Bundesland") +
  #     #breaks=c(""), 
  #     #labels= c("")) +
  #     labs(
  #       x = "Wetter",
  #       y = "Tage bzw Temperatur",
  #       caption = "Quelle & Copyright: Statistisches Bundesamt") +
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # })
  # 
  # output$Wetter1.2 <- DT::renderDT({
  #   Wetter_final %>%
  #     filter(Bundesland == input$Bundesland3.2) %>%
  #     filter(Jahr == input$Jahr3.2)
  # })
  
  output$Wahl3.1.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.1, ") nach Mostsorte für", input$Bundesland3.1.1, "im Jahr", input$Jahr3.1.1, ".")
  })
  
  output$Weinernte1.1 <- renderPlot(width = "auto", height = 600, {
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.1.1) %>%
      filter(Jahr == input$Jahr3.1.1) %>%
      filter(Messparameter == input$Messparameter3.1) %>%
      ggplot() +
      aes(x = Mostsorte, y = Wert) +
      geom_col(position = "dodge", fill = c("#8aa4be", "#9e0657", "#2c3e50")) +
      geom_label(aes(label=Wert)) + 
      ylim(0, 1000000) +
      labs(
        x = "Mostsorte",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
  })
  
  output$Weiernte1.2 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.1.1) %>%
      filter(Jahr == input$Jahr3.1.1) %>%
      filter(Messparameter == input$Messparameter3.1)
  })
  
  output$Wahl3.1.2 <- renderText({
    paste("Wetter nach Wetterphänomenen für", input$Bundesland3.1.2, "im Jahr", input$Jahr3.1.2, ".")
  })
  
  output$Wetter1.1 <- renderPlot(width = "auto", height = 600, {
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.1.2) %>%
      filter(Jahr == input$Jahr3.1.2) %>%
      ggplot() +
      aes(x = Wetterphänomen, y = Wert) +
      geom_col(position = "dodge") +
      geom_label(aes(label=Wert)) + 
      #ylim(0, 1000) +
      labs(
        x = "Wetterphänomene",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter1.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.1.2) %>%
      filter(Jahr == input$Jahr3.1.2)
  })
  
  output$Wahl3.2.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.2.1, ") nach Mostsorte für", input$Bundesland3.2.1, "zwischen 1993 und 2018.")
  })
  
  output$Weinernte2.1 <- renderPlot({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.1) %>%
      filter(Messparameter == input$Messparameter3.2.1) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Mostsorte)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinernte2.2 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.1) %>%
      filter(Messparameter == input$Messparameter3.2.1)
  })
  
  output$Wahl3.2.2 <- renderText({
    paste("Wetter nach Wetterphänomenen für", input$Bundesland3.2.2, "zwischen 1993 und 2018.")
  })
  
  output$Wetter2.1 <- renderPlot({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.2) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Wetterphänomen)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter2.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.2)
  })
  
  output$Wahl3.2.3 <- renderText({
    paste("Weinernte (", input$Messparameter3.2.2, ") der Mostsorte", input$Mostsorte3.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland3.2.3, "und", input$Bundesland3.2.4, ".")
  })
  
  output$Weinernte2.3 <- renderPlot({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.3 | Bundesland == input$Bundesland3.2.4) %>%
      filter(Messparameter == input$Messparameter3.2.2) %>%
      filter(Mostsorte == input$Mostsorte3.2) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Bundesland)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinernte2.4 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.3 | Bundesland == input$Bundesland3.2.4) %>%
      filter(Messparameter == input$Messparameter3.2.2) %>%
      filter(Mostsorte == input$Mostsorte3.2)
  })
  
  output$Wahl3.2.4 <- renderText({
    paste(input$Wetterphänomen3.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland3.2.5, "und", input$Bundesland3.2.6, ".")
  })
  
  output$Wetter2.3 <- renderPlot({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.5 | Bundesland == input$Bundesland3.2.6) %>%
      filter(Wetterphänomen == input$Wetterphänomen3.2) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Bundesland)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter2.4 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.5 | Bundesland == input$Bundesland3.2.6) %>%
      filter(Wetterphänomen == input$Wetterphänomen3.2)
  })
  
  output$Wahl3.3.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.3.1, ") der Bundesländer für die Mostsorte", input$Rebsorte3.3.1, "im Jahr", input$Jahr3.3.1, ".")
  })
  
  output$Weinernte3.1 <- renderPlot({
    E_BL_Jahr_RS_neu %>%
      filter(Mostsorte == input$Mostsorte3.3.1) %>%
      filter(Jahr == input$Jahr3.3.1) %>%
      filter(Messparameter == input$Messparameter3.3.1) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinernte3.2 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1)
  })
  
  output$Wahl3.3.2 <- renderText({
    paste(input$Wetterphänomen3.3.1, "der Bundesländer im Jahr", input$Jahr3.3.2, ".")
  })
  
  output$Wetter3.1 <- renderPlot({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.1) %>%
      filter(Jahr == input$Jahr3.3.2) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Wetter3.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.1) %>%
      filter(Jahr == input$Jahr3.3.2)
  })
  
  output$Wahl3.3.3 <- renderText({
    paste("Weinernte (", input$Messparameter3.3.2, ") der Bundesländer für  die Mostsorte", input$Mostsorte3.3.2, "im Vergleich der Jahre", input$Jahr3.3.3, "und", input$Jahr3.3.4, ".")
  })
  
  output$Weinbestand3.3 <- renderPlot({
    E_BL_Jahr_RS_neu %>%
      filter(Messparameter == input$Messparameter3.3.2) %>%
      filter(Mostsorte == input$Mostsorte3.3.2) %>%
      filter(Jahr == input$Jahr3.3.3 | Jahr == input$Jahr3.3.4) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert, fill = Jahr)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinbestand3.4 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Messparameter == input$Messparameter3.3.2) %>%
      filter(Mostsorte == input$Mostsorte3.3.2) %>%
      filter(Jahr == input$Jahr3.3.3 | Jahr == input$Jahr3.3.4)
  })
  
  output$Wahl3.3.4 <- renderText({
    paste(input$Wetterphänomen3.3.2, "der Bundesländer im Vergleich der Jahre", input$Jahr3.3.5, "und", input$Jahr3.3.6, ".")
  })
  
  output$Wetter3.3 <- renderPlot({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.2) %>%
      filter(Jahr == input$Jahr3.3.5 | Jahr == input$Jahr3.3.6) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert, fill = Jahr)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Wetter3.4 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.2) %>%
      filter(Jahr == input$Jahr3.3.5 | Jahr == input$Jahr3.3.6)
  })
  
  
  # tabPanel 4 - Weinproduktion ----
  output$Wahl4.1 <- renderText({
    paste("Weinbestand (in hl) nach Weinkategorie für", input$Bundesland4.1, "im Jahr", input$Jahr4.1, ".")
  })
  
  output$Weinproduktion1.1 <- renderPlot(width = "auto", height = 600, {
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.1) %>%
      filter(Jahr == input$Jahr4.1) %>%
      ggplot() +
      aes(x = Weinkategorie, y = hl) +
      geom_col(position = "dodge") +
      geom_label(aes(label=hl)) + 
      ylim(0, 6800000) +
      labs(
        x = "Weinkategorie",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion1.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.1) %>%
      filter(Jahr == input$Jahr4.1)
  })
  
  output$Wahl4.2.1 <- renderText({
    paste("Weinbestand (in hl) nach Weinkategorie für", input$Bundesland4.2.1, "zwischen 2010 und 2018.")
  })
  
  output$Weinproduktion2.1 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.1) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Weinkategorie)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion2.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.1)
  })
  
  output$Wahl4.2.2 <- renderText({
    paste("Weinbestand (in hl) der Weinkategorie", input$Weinkategorie4.2, "zwischen 2010 und 2018 im Vergleich von", input$Bundesland4.2.2, "und", input$Bundesland4.2.3, ".")
  })
  
  output$Weinproduktion2.3 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.2 | Bundesland == input$Bundesland4.2.3) %>%
      filter(Weinkategorie == input$Weinkategorie4.2) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Bundesland)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion2.4 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.2 | Bundesland == input$Bundesland4.2.3) %>%
      filter(Weinkategorie == input$Weinkategorie4.2)
  })
  
  output$Wahl4.3.1 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Weinkategorie", input$Weinkategorie4.3.1, "im Jahr", input$Jahr4.3.1, ".")
  })
  
  output$Weinproduktion3.1 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.1) %>%
      filter(Jahr == input$Jahr4.3.1) %>%
      ggplot()+
      aes(x = Bundesland, y = hl)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinproduktion3.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.1) %>%
      filter(Jahr == input$Jahr4.3.1)
  })
  
  output$Wahl4.3.2 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Weinkategorie", input$Weinkategorie4.3.2, "im Vergleich der Jahre", input$Jahr4.3.2, "und", input$Jahr4.3.3, ".")
  })
  
  output$Weinproduktion3.3 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.2) %>%
      filter(Jahr == input$Jahr4.3.2 | Jahr == input$Jahr4.3.3) %>%
      ggplot()+
      aes(x = Bundesland, y = hl, fill = Jahr)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinproduktion3.4 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.2) %>%
      filter(Jahr == input$Jahr4.3.2 | Jahr == input$Jahr4.3.3)
  })
  
  
  
  
  # tabPanel 5 - Weinbestände ----
  output$Wahl5.1 <- renderText({
    paste("Weinbestand (in hl) nach Rebsorten für", input$Bundesland5.1, "im Jahr", input$Jahr5.1, ".")
  })
  
  output$Weinbestand1.1 <- renderPlot(width = "auto", height = 600, {
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.1) %>%
      filter(Jahr == input$Jahr5.1) %>% 
      ggplot() +
      aes(x = Rebsorte, y = hl) +
      geom_col(position = "dodge", fill = c("#8aa4be", "#9e0657", "#2c3e50")) +
      geom_label(aes(label=hl)) + 
      ylim(0, 10000000) +
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
  
  output$Wahl5.2.1 <- renderText({
    paste("Weinbestand (in hl) nach Rebsorten für", input$Bundesland5.2.1, "zwischen 1993 und 2018.")
  })
  
  output$Weinbestand2.1 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.1) %>%
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
      filter(Bundesland == input$Bundesland5.2.1)
  })
  
  output$Wahl5.2.2 <- renderText({
    paste("Weinbestand (in hl) der Rebsorte", input$Rebsorte5.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland5.2.2, "und", input$Bundesland5.2.3, ".")
  })
  
  output$Weinbestand2.3 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.2 | Bundesland == input$Bundesland5.2.3) %>%
      filter(Rebsorte == input$Rebsorte5.2) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Bundesland)+
      geom_point()+
      geom_line()+
      labs(
        x = "Jahr",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinbestand2.4 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.2 | Bundesland == input$Bundesland5.2.3) %>%
      filter(Rebsorte == input$Rebsorte5.2)
  })
  
  output$Wahl5.3.1 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Rebsorte", input$Rebsorte5.3.1, "im Jahr", input$Jahr5.3.1, ".")
  })
  
  output$Weinbestand3.1 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1) %>%
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
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1)
  })
  
  output$Wahl5.3.2 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für  die Rebsorte", input$Rebsorte5.3.2, "im Vergleich der Jahre", input$Jahr5.3.2, "und", input$Jahr5.3.3, ".")
  })
  
  output$Weinbestand3.3 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.2) %>%
      filter(Jahr == input$Jahr5.3.2 | Jahr == input$Jahr5.3.3) %>%
      ggplot()+
      aes(x = Bundesland, y = hl, fill = Jahr)+
      geom_col(position = "dodge")+
      labs(
        x = "Bundesland",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinbestand3.4 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.2) %>%
      filter(Jahr == input$Jahr5.3.2 | Jahr == input$Jahr5.3.3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)