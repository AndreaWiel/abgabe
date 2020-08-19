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
library(shinyWidgets)
library(dplyr)

# Daten einlesen ----
E_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Erntemenge_Bundeslaender_Jahr_Rebsorte.csv", na.strings=c("","NA"), dec = ".")
RF_ABG_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Rebflaechen_Anbaugebiete_Jahr_Rebsorte.csv", na.strings=c("","NA"), check.names = FALSE, dec = ".")
WB_BL_Jahr_RS <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinbestaende_Bundeslaender_Jahre_Rebsorte.csv", na.strings=c("","NA"), check.names = FALSE, dec = ".")
WP_BL_Jahr_WK <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wein/Weinproduktion_Bundeslaender_Jahre_Rebsorte.csv", na.strings=c("","NA"), dec = ".")
Frosttage <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Frosttage_Bundeslaender_Jahr_neu.csv", na.strings=c("","NA"), dec = ".")
Sommertage <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sommertage_Bundeslaender_Jahr_neu.csv", na.strings=c("","NA"), dec = ".")
Sonnenstunden <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnitt_Sonnenstunden_Bundeslaender_Jahr_neu.csv", na.strings=c("","NA"), dec = ".")
Regen <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittsniederschlag_Bundeslaender_Jahr_neu.csv", na.strings=c("","NA"),  dec = ".")
TempDurch <- read.csv2("https://raw.githubusercontent.com/AndreaWiel/abgabe/master/WineTime/csv_Datensaetze/Wetter/Durchschnittstemperatur_Bundeslaender_Jahr_neu.csv", na.strings=c("","NA"), dec = ".")

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

E_BL_Op <- E_BL_Jahr_RS_neu$Bundesland %>% unique()
E_MS_Op <- E_BL_Jahr_RS_neu$Mostsorte %>% unique()
E_MP_Op <- E_BL_Jahr_RS_neu$Messparameter %>% unique()


## Daten Weinproduktion
WP_BL_Jahr_WK_neu <- WP_BL_Jahr_WK %>%
  rename(c("Weißwein: Qualitätswein" = "Weisswein...Qualitaetswein",
           "Weißwein: Prädikatswein" = "Weisswein...Praedikatswein",
           "Weißwein: Wein und/oder Landwein" = "Weisswein...Wein.und.oder.Landwein",
           "Weißwein: Insgesamt" = "Weisswein...Insgesamt",
           "Rotwein: Qualitätswein" = "Rotwein...Qualitaetswein",
           "Rotwein: Prädikatswein" = "Rotwein...Praedikatswein",
           "Rotwein: Wein und/oder Landwein" = "Rotwein....Wein.und.oder.Landwein",
           "Rotwein: Insgesamt" = "Rotwein...Insgesamt",
           "Insgesamt: Qualitätswein" = "insgesamt...Qualitaetswein",
           "Insgesamt: Prädikatswein" = "Insgesamt...Praedikatswein",
           "Insgesamt: Wein und/oder Landwein" = "Insgesamt...Wein.und.oder.Landwein",
           "Insgesamt: alle Rebsorten und Weinkategorien" = "Insgesamt...Rebsorten")) %>%
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
           "Baden-Württemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen",
           "Gesamt-Deutschland" = "Gesamt.Deutschland")) %>%
  gather("Bundesland", "Frosttage", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Regenmenge
Regen_neu <- Regen %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Württemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen",
           "Gesamt-Deutschland" = "Gesamt.Deutschland")) %>%
  gather("Bundesland", "Regenmenge in mm (1mm = 1l/m²)", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Sonnenstunden
Sonnenstunden_neu <- Sonnenstunden %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Württemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen",
           "Gesamt-Deutschland" = "Gesamt.Deutschland")) %>%
  gather("Bundesland", "Sonnenstunden", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Sommertage
Sommertage_neu <- Sommertage %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Württemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen",
           "Gesamt-Deutschland" = "Gesamt.Deutschland")) %>%
  gather("Bundesland", "Sommertage", 2:18) %>%
  filter(Jahr >= 1993)


## Daten Temperatur
TempDurch_neu <- TempDurch %>%
  rename(c("Brandenburg & Berlin" = "Brandenburg.Berlin", 
           "Baden-Württemberg" = "Baden.Wuerttemberg", 
           "Mecklenburg-Vorpommern" = "Mecklenburg.Vorpommern", 
           "Niedersachsen, Hamburg & Bremen" = "Niedersachsen.Hamburg.Bremen", 
           "Nordrhein-Westfalen" = "Nordrhein.Westfalen",
           "Rheinland-Pfalz" = "Rheinland.Pfalz",
           "Schleswig-Holstein" = "Schleswig.Holstein",
           "Sachsen-Anhalt" = "Sachsen.Anhalt",
           "Thüringen & Sachsen-Anhalt" = "Thueringen.Sachsen.Anhalt",
           "Thüringen" = "Thueringen",
           "Gesamt-Deutschland" = "Gesamt.Deutschland")) %>%
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


# Define UI ----
ui <- navbarPage(title = div(img(src='images/Weinglas_weiß.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60), "WineTime"),
                 theme = "bootstrap.css",
                 footer = includeHTML("footer.html"),
                 fluid = TRUE, 
                 collapsible = TRUE,
                 
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
                            tabPanel("Gut zu Wissen",
                                     includeHTML("WeinanbauINFO.html")
                            ),
                            tabPanel("Die deutschen Anbaugebiete",
                                     includeHTML("Weinanbau.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr2.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Anbaugebiet2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1]),
                                                    h6("Hinweis: Mittelrhein: Rheinland-Pfalz. Nordrhein-Westfalen.
                                                                     Mosel: Rheinland-Pfalz. Saarland. Bis 2007 Anbaugebiet Mosel-Saar-Ruwer
                                                                    Saale-Unstrut: Brandenburg. Sachsen-Anhalt. Thüringen.
                                                                    Sachsen: Brandenburg. Sachsen-Anhalt. Sachsen.")
                                       ),
                                       mainPanel(h4(strong("Die deutschen Anbaugebiete")),
                                                 textOutput('Wahl2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinanbaugebiete1.1', height = 650)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Anbaugebiet2.2.1", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1]),
                                                    h6("Hinweis: Mittelrhein: Rheinland-Pfalz. Nordrhein-Westfalen.
                                                                     Mosel: Rheinland-Pfalz. Saarland. Bis 2007 Anbaugebiet Mosel-Saar-Ruwer
                                                                    Saale-Unstrut: Brandenburg. Sachsen-Anhalt. Thüringen.
                                                                    Sachsen: Brandenburg. Sachsen-Anhalt. Sachsen.")
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                                 textOutput('Wahl2.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinanbaugebiete2.1', height = 650)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinanbaugebiete2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Anbaugebiet2.2.2", "Wählen Sie ein Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[1]),
                                                    selectInput("Anbaugebiet2.2.3", "Wählen Sie ein weiteres Anbaugebiet:", choices = RF_ABG_Op, selected = RF_ABG_Op[2]),
                                                    selectInput("Rebsorte2.2", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1]),
                                                    h6("Hinweis: Mittelrhein: Rheinland-Pfalz. Nordrhein-Westfalen.
                                                                     Mosel: Rheinland-Pfalz. Saarland. Bis 2007 Anbaugebiet Mosel-Saar-Ruwer
                                                                    Saale-Unstrut: Brandenburg. Sachsen-Anhalt. Thüringen.
                                                                    Sachsen: Brandenburg. Sachsen-Anhalt. Sachsen.")
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Zeitvergleich")),
                                                 textOutput('Wahl2.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinanbaugebiete2.3', height = 650)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Rebsorte2.3.1", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1]),
                                                    h6("Hinweis: Mittelrhein: Rheinland-Pfalz. Nordrhein-Westfalen.
                                                                     Mosel: Rheinland-Pfalz. Saarland. Bis 2007 Anbaugebiet Mosel-Saar-Ruwer
                                                                    Saale-Unstrut: Brandenburg. Sachsen-Anhalt. Thüringen.
                                                                    Sachsen: Brandenburg. Sachsen-Anhalt. Sachsen.")
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                                 textOutput('Wahl2.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinanbaugebiete3.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinanbaugebiete3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr2.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr2.3.3", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte2.3.2", "Wählen Sie eine Rebsorte:", choices = RF_RS_Op, selected = RF_RS_Op[1]),
                                                    h6("Hinweis:    Mittelrhein: Rheinland-Pfalz. Nordrhein-Westfalen.
                                                                     Mosel: Rheinland-Pfalz. Saarland. Bis 2007 Anbaugebiet Mosel-Saar-Ruwer
                                                                    Saale-Unstrut: Brandenburg. Sachsen-Anhalt. Thüringen.
                                                                    Sachsen: Brandenburg. Sachsen-Anhalt. Sachsen.")
                                       ),
                                       mainPanel(h4(strong("Anbaugebiete im Ländervergleich")),
                                                 textOutput('Wahl2.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinanbaugebiete3.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinanbaugebiete3.4')
                                                   )
                                                 )
                                       )
                                     )
                            )
                 ),
                 
                 # tabPanel 3 - Ernte ----
                 navbarMenu("Weinernte",
                            tabPanel("Gut zu Wissen",
                                     includeHTML("WeinernteINFO.html")
                            ),
                            tabPanel("Weinernte & Wetter der Bundesländer",
                                     includeHTML("Weinernte.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.1.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland3.1.1", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Messparameter3.1", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen ab 1998.
                                                       Rotmost: Einschließlich Most aus gemischten Beständen.")
                                       ),
                                       mainPanel(h4(strong("Weinernte der Bundesländer")),
                                                 textOutput('Wahl3.1.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinernte1.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinernte1.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.1.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland3.1.2", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    h6("*Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetterdaten der Bundesländer")),
                                                 textOutput('Wahl3.1.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Wetter1.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Messparameter3.2.1", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen ab 1998.
                                                    Rotmost: Einschließlich Most aus gemischten Beständen.")
                                       ),
                                       mainPanel(h4(strong("Weinernte im Zeitvergleich")),
                                                 textOutput('Wahl3.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinernte2.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinernte2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.2", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    h6("*Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetter im Zeitvergleich")),
                                                 textOutput('Wahl3.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Wetter2.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Wetter2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.3", "Wählen Sie ein Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Bundesland3.2.4", "Wählen Sie ein weiteres Bundesland:", choices = E_BL_Op, selected = E_BL_Op[1]),
                                                    selectInput("Messparameter3.2.2", "Wählen Sie ein Messparameter", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    selectInput("Mostsorte3.2", "Wählen Sie eine Mostsorte", choices = E_MS_Op, selected = E_MS_Op),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen ab 1998.
                                                    Rotmost: Einschließlich Most aus gemischten Beständen.")
                                       ),
                                       mainPanel(h4(strong("Weinernte im Zeitvergleich")),
                                                 textOutput('Wahl3.2.3'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinernte2.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinernte2.4')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland3.2.5", "Wählen Sie ein Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[1]),
                                                    selectInput("Bundesland3.2.6", "Wählen Sie ein weiteres Bundesland*:", choices = Wetter_BL_Op, selected = Wetter_BL_Op[2]),
                                                    selectInput("Wetterphänomen3.2", "Wählen Sie eine Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1]),
                                                    h6("Hinweis: Die Stadtstaaten Berlin, Bremen und Hamburg können aufgrund nicht ausreichend differenzierter Daten leider nicht einzeln ausgewiesen werden. Die Wetterdaten für Berlin können nur in Verbindung mit Brandenburg betrachtet werden, sowie die Wetterdaten für Bremen und Hamburg nur in Verbindung mit Niedersachsen.")
                                       ),
                                       mainPanel(h4(strong("Wetter im Zeitvergleich")),
                                                 textOutput('Wahl3.2.4'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Wetter2.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Mostsorte3.3.1", "Wählen Sie eine Mostsorte:", choices = E_MS_Op, selected = E_MS_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen ab 1998.
                                                      Rotmost: Einschließlich Most aus gemischten Beständen.")
                                       ),
                                       mainPanel(h4(strong("Weinernte im Ländervergleich")),
                                                 textOutput('Wahl3.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinernte3.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinernte3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    selectInput("Wetterphänomen3.3.1", "Wählen Sie eine Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Wetter im Ländervergleich")),
                                                 textOutput('Wahl3.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Wetter3.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Wetter3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.3", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr3.3.4", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Messparameter3.3.2", "Wählen Sie ein Messparameter:", choices = E_MP_Op, selected = E_MP_Op[1]),
                                                    selectInput("Mostsorte3.3.2", "Wählen Sie eine Mostsorte:", choices = E_MS_Op, selected = E_MS_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen ab 1998.
                                                    Rotmost: Einschließlich Most aus gemischten Beständen.")
                                       ),
                                       mainPanel(h4(strong("Weinernte im Ländervergleich")),
                                                 textOutput('Wahl3.3.3'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinernte3.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinernte3.4')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr3.3.5", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr3.3.6", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Wetterphänomen3.3.2", "Wählen Sie ein Wetterphänomen:", choices = Wetter_WP_Op, selected = Wetter_WP_Op[1])
                                       ),
                                       mainPanel(h4(strong("Wetter im Ländervergleich")),
                                                 textOutput('Wahl3.3.4'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Wetter3.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Wetter3.4')
                                                   )
                                                 )
                                       )
                                     )
                            )
                            
                 ),
                 
                 # tabPanel 4 - Weinproduktion ----
                 navbarMenu("Weinproduktion",
                            tabPanel("Gut zu Wissen",
                                     includeHTML("WeinproduktionINFO.html")
                            ),
                            tabPanel("Weinproduktion der Bundesländer",
                                     includeHTML("Weinproduktion.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr4.1", "Wählen Sie ein Jahr:", min = 2010, max = 2018, value = 2012, step = 1, sep = ""),
                                                    selectInput("Bundesland4.1", "Wählen Sie ein Bundesland:", choices = WP_BL_Op, selected = WP_BL_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen.    Rotwein: Einschließlich Rotling und Roséwein.")
                                                     ),
                                       mainPanel(h4(strong("Weinbestände der Bundesländer")),
                                                 textOutput('Wahl4.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinproduktion1.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Bundesland4.2.1", "Wählen Sie ein Bundesland:", choices = WP_BL_Op, selected = WP_BL_Op[1]),
                                                    checkboxGroupInput("Weinkategorie", "Wählen Sie verschiedene Weinkategorien:", choices = WP_WK_Op, selected = WP_WK_Op[1])
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Zeitvergleich")),
                                                 textOutput('Wahl4.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinproduktion2.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinproduktion2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland4.2.2", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    selectInput("Bundesland4.2.3", "Wählen Sie ein weiteres Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[2]),
                                                    selectInput("Weinkategorie4.2", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen.    Rotwein: Einschließlich Rotling und Roséwein.")
                                                     ),
                                      
                                       mainPanel(h4(strong("Weinproduktion im Zeitvergleich")),
                                                 textOutput('Wahl4.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinproduktion2.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Weinkategorie4.3.1", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen.    Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Ländervergleich")),
                                                 textOutput('Wahl4.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinproduktion3.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinproduktion3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr4.3.2", "Wählen Sie ein Jahr:", min = 2010, max = 2018, value = 2012, step = 1, sep = ""),
                                                    sliderInput("Jahr4.3.3", "Wählen Sie ein weiteres Jahr:", min = 2010, max = 2018, value = 2015, step = 1, sep = ""),
                                                    selectInput("Weinkategorie4.3.2", "Wählen Sie eine Weinkategorie:", choices = WP_WK_Op, selected = WP_WK_Op[1]),
                                                    h6("Hinweis: Sachsen-Anhalt: Einschließlich Thüringen.    Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinproduktion im Ländervergleich")),
                                                 textOutput('Wahl4.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinproduktion3.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinproduktion3.4')
                                                   )
                                                 )
                                       )
                                     )
                            )
                            
                 ),
                 
                 # tabPanel 5 - Weinbestände ----
                 navbarMenu("Weinbestände",
                            tabPanel("Gut zu Wissen",
                                     includeHTML("WeinbestandINFO.html")
                            ),
                            tabPanel("Weinbestände der Bundesländer",
                                     includeHTML("Weinbestand.html"),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr5.1", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Bundesland5.1", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    h6("Hinweis: Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinbestände der Bundesländer")),
                                                 textOutput('Wahl5.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinbestand1.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Bundesland5.2.1", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    h6("Hinweis: Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Zeitvergleich")),
                                                 textOutput('Wahl5.2.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinbestand2.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinbestand2.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    selectInput("Bundesland5.2.2", "Wählen Sie ein Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[1]),
                                                    selectInput("Bundesland5.2.3", "Wählen Sie ein weiteres Bundesland:", choices = WB_BL_Op, selected = WB_BL_Op[2]),
                                                    selectInput("Rebsorte5.2", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1]),
                                                    h6("Hinweis: Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Zeitvergleich")),
                                                 textOutput('Wahl5.2.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinbestand2.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
                                                    selectInput("Rebsorte5.3.1", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1]),
                                                    h6("Hinweis: Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Ländervergleich")),
                                                 textOutput('Wahl5.3.1'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinbestand3.1', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
                                                            DT::DTOutput('Weinbestand3.2')
                                                   )
                                                 )
                                       )
                                     ),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(h4(strong("Auswahlmöglichkeiten")),
                                                    sliderInput("Jahr5.3.2", "Wählen Sie ein Jahr:", min = 1993, max = 2018, value = 2005, step = 1, sep = ""),
                                                    sliderInput("Jahr5.3.3", "Wählen Sie ein weiteres Jahr:", min = 1993, max = 2018, value = 2010, step = 1, sep = ""),
                                                    selectInput("Rebsorte5.3.2", "Wählen Sie eine Rebsorte:", choices = WB_RS_Op, selected = WB_RS_Op[1]),
                                                    h6("Hinweis: Rotwein: Einschließlich Rotling und Roséwein.")
                                       ),
                                       mainPanel(h4(strong("Weinbestände im Ländervergleich")),
                                                 textOutput('Wahl5.3.2'),
                                                 tabsetPanel(
                                                   tabPanel("Grafik", icon = icon("bar-chart-o"),
                                                            plotOutput('Weinbestand3.3', height = 750)
                                                   ),
                                                   tabPanel("Tabelle", icon = icon("table"),
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
  ## Anbau 1 ----
  output$Wahl2.1 <- renderText({
    paste("Weinanbaufläche (in ha) nach Rebsorten für das Anbaugebiet", input$Anbaugebiet2.1, "im Jahr", input$Jahr2.1, ".")
  })
  
  output$Weinanbaugebiete1.1 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
      filter(Jahr == input$Jahr2.1) %>%
      ggplot() +
      aes(x = Rebsorte, y = ha) +
      geom_col(position = "dodge", fill = c("Weisswein" = "#8aa4be", "Rotwein" = "#9e0657", "Insgesamt" = "#2c3e50")) +
      geom_label(aes(label=ha)) + 
      scale_y_continuous(limits = c(0, 106300), breaks = seq(0, 106300, by = 10000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Rebsorten",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
  })
  
  output$Weinanbaugebiete1.2 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.1) %>%
      filter(Jahr == input$Jahr2.1)
  })
  
  ## Anbau 2.1 ----
  output$Wahl2.2.1 <- renderText({
    paste("Weinanbaufläche (in ha) nach Rebsorten für das Anbaugebiet", input$Anbaugebiet2.2.1, "zwischen 1993 und 2018.")
  })
  
  output$Weinanbaugebiete2.1 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.1) %>%
      ggplot()+
      aes(x = Jahr, y = ha, color = Rebsorte)+
      geom_point(size = 2) +
      geom_line(aes(group = Rebsorte), size = 1.25)+
      scale_color_manual(values = c("Weisswein" = "#8aa4be", "Rotwein" = "#9e0657", "Insgesamt" = "#2c3e50")) +
      scale_y_continuous(limits = c(0, 106300), breaks = seq(0, 106300, by = 10000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinanbaugebiete2.2 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.1)
  })
  
  ## Anbau 2.2 ----
  output$Wahl2.2.2 <- renderText({
    paste("Weinanbaufläche (in ha) der Rebsorte", input$Rebsorte2.2, "zwischen 1993 und 2018 im Vergleich der Anbaugebiete", input$Anbaugebiet2.2.2, "und", input$Anbaugebiet2.2.3, ".")
  })
  
  output$Weinanbaugebiete2.3 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.2 | Anbaugebiet == input$Anbaugebiet2.2.3) %>%
      filter(Rebsorte == input$Rebsorte2.2) %>%
      ggplot()+
      aes(x = Jahr, y = ha, color = Anbaugebiet)+
      geom_point(size = 2)+
      geom_line(aes(group = Anbaugebiet), size = 1.25)+
      scale_color_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 106300), breaks = seq(0, 106300, by = 10000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinanbaugebiete2.4 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Anbaugebiet == input$Anbaugebiet2.2.2 | Anbaugebiet == input$Anbaugebiet2.2.3) %>%
      filter(Rebsorte == input$Rebsorte2.2)
  })
  
  ## Anbau 3.1 ----
  output$Wahl2.3.1 <- renderText({
    paste("Weinanbaufläche (in ha) der deutschen Anbaugebiete für die Rebsorte", input$Rebsorte2.3.2, "im Jahr", input$Jahr2.3.1, ".")
  })
  
  output$Weinanbaugebiete3.1 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte2.3.1) %>%
      filter(Jahr == input$Jahr2.3.1) %>%
      ggplot()+
      aes(x = Anbaugebiet, y = ha)+
      geom_col(position = "dodge", fill = "#2c3e50")+
      geom_label(aes(label=ha)) +
      scale_y_continuous(limits = c(0, 106300), breaks = seq(0, 106300, by = 10000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Anbaugebiete",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinanbaugebiete3.2 <- DT::renderDT({
    RF_ABG_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte2.3.1) %>%
      filter(Jahr == input$Jahr2.3.1)
  })
  
  ## Anbau 3.2 ----
  output$Wahl2.3.2 <- renderText({
    paste("Weinanbaufläche (in ha) der deutschen Anbaugebiete für die Rebsorte", input$Rebsorte2.3.2, "im Vergleich der Jahre", input$Jahr2.3.2, "und", input$Jahr2.3.3, ".")
  })
  
  output$Weinanbaugebiete3.3 <- renderPlot({
    RF_ABG_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte2.3.2) %>%
      filter(Jahr == input$Jahr2.3.2 | Jahr == input$Jahr2.3.3) %>%
      ggplot()+
      aes(x = Anbaugebiet, y = ha) +
      geom_col(aes(fill = Jahr), position = "dodge") +
      geom_label(aes(label=ha, group = Jahr), position = position_dodge(1), size = 3) +
      scale_fill_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 106300), breaks = seq(0, 106300, by = 10000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Anbaugebiete",
        y = "Anbaufläche in ha",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
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
  ## Weinernte 1 ----
  output$Wahl3.1.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.1, ") nach Mostsorte für", input$Bundesland3.1.1, "im Jahr", input$Jahr3.1.1, ".")
  })
  
  output$Weinernte1.1 <- renderPlot({
    Ernte1 <- E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.1.1) %>%
      filter(Jahr == input$Jahr3.1.1) %>%
      filter(Messparameter == input$Messparameter3.1) 
    
    AErnte1 <- NULL
    STErnte1 <- Ernte1 %>% 
      group_by(Bundesland) %>% 
      summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
      pull(Wert)
    if(STErnte1 == 0) {
      AErnte1 <- annotate("text", x = 2, y = 6000000, label = "In diesem Bundesland gab es entweder keine Weinernte oder es liegen keine Daten vor.", size = 5)
    }
    
    Ernte1 %>%
      ggplot() +
      aes(x = Mostsorte, y = Wert) +
      geom_col(position = "dodge", fill = c("Weißmost" = "#8aa4be", "Rotmost" = "#9e0657", "Weinmost insgesamt" = "#2c3e50")) +
      geom_label(aes(label=Wert)) + 
      scale_y_continuous(limits = c(0, 12300970), breaks = seq(0, 12300970, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Mostsorte",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
      AErnte1
  })
  
  output$Weiernte1.2 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.1.1) %>%
      filter(Jahr == input$Jahr3.1.1) %>%
      filter(Messparameter == input$Messparameter3.1)
  })
  
  ## Wetter 1 ----
  output$Wahl3.1.2 <- renderText({
    paste("Wetter nach Wetterphänomenen für", input$Bundesland3.1.2, "im Jahr", input$Jahr3.1.2, ".")
  })
  
  output$Wetter1.1 <- renderPlot({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.1.2) %>%
      filter(Jahr == input$Jahr3.1.2) %>%
      ggplot() +
      aes(x = Wetterphänomen, y = Wert) +
      geom_col(position = "dodge", fill = c("Frosttage" = "#4c7cb0", "Regenmenge in mm (1mm = 1l/m²)" = "#36587d", "Sommertage" = "#6db2fc", "Sonnenstunden" = "#b9dafd", "Temperaturdurchschnitt in °C" = "#5b6c7d")) +
      geom_label(aes(label=Wert)) + 
      scale_y_continuous(limits = c(0, 2160), breaks = seq(0, 2160, by = 100), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Wetterphänomene",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Deutscher Wetterdienst 2020 | Stand: 18.08.2020") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter1.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.1.2) %>%
      filter(Jahr == input$Jahr3.1.2)
  })
  
  ## Weinernte 2.1 ----
  output$Wahl3.2.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.2.1, ") nach Mostsorte für", input$Bundesland3.2.1, "zwischen 1993 und 2018.")
  })
  
  output$Weinernte2.1 <- renderPlot({
     Ernte2.1 <- E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.1) %>%
      filter(Messparameter == input$Messparameter3.2.1) 
     
     
     AErnte2.1 <- NULL
     STErnte2.1 <- Ernte2.1 %>% 
       group_by(Bundesland) %>% 
       summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
       pull(Wert)
     if(STErnte2.1 == 0) {
       AErnte2.1 <- annotate("text", x = 2005, y = 6000000, label = "In diesem Bundesland gab es entweder keine Weinernte oder es liegen keine Daten vor.", size = 4)
     }
     
     Ernte2.1 %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Mostsorte)+
      geom_point(size = 2)+
      geom_line(size = 1.25)+
      scale_color_manual(values = c("Weißmost" = "#8aa4be", "Rotmost" = "#9e0657", "Weinmost insgesamt" = "#2c3e50")) +
      scale_y_continuous(limits = c(0, 12300970), breaks = seq(0, 12300970, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahr",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.202")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
       AErnte2.1
  })
  
  output$Weinernte2.2 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.1) %>%
      filter(Messparameter == input$Messparameter3.2.1)
  })
  
  ## Wetter 2.1 ----
  output$Wahl3.2.2 <- renderText({
    paste("Wetter nach Wetterphänomenen für", input$Bundesland3.2.2, "zwischen 1993 und 2018.")
  })
  
  output$Wetter2.1 <- renderPlot({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.2) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Wetterphänomen)+
      geom_point(size = 2)+
      geom_line(size = 1.25)+
      scale_color_manual(values = c("Frosttage" = "#4c7cb0", "Regenmenge in mm (1mm = 1l/m²)" = "#36587d", "Sommertage" = "#6db2fc", "Sonnenstunden" = "#b9dafd", "Temperaturdurchschnitt in °C" = "#5b6c7d")) +
      scale_y_continuous(limits = c(0, 2160), breaks = seq(0, 2160, by = 100), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahr",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Deutscher Wetterdienst 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter2.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.2)
  })
  
  ## Weinernte 2.2 ----
  output$Wahl3.2.3 <- renderText({
    paste("Weinernte (", input$Messparameter3.2.2, ") der Mostsorte", input$Mostsorte3.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland3.2.3, "und", input$Bundesland3.2.4, ".")
  })
  
  output$Weinernte2.3 <- renderPlot({
    Ernte2.3 <- E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.3 | Bundesland == input$Bundesland3.2.4) %>%
      filter(Messparameter == input$Messparameter3.2.2) %>%
      filter(Mostsorte == input$Mostsorte3.2)
    
    AErnte2.3 <- NULL
    STErnte2.3 <- Ernte2.3 %>% 
      group_by(Bundesland) %>% 
      summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
      pull(Wert)
    if(STErnte2.3 == 0) {
      AErnte2.3 <- annotate("text", x = 1993, y = 6000000, label = "In diesem Bundesland gab es entweder keine Weinernte oder es liegen keine Daten vor.", size = 4)
    }
    Ernte2.3 %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Bundesland)+
      geom_point(size = 2)+
      geom_line(size = 1.25)+
      scale_color_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 12300970), breaks = seq(0, 12300970, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahr",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.202")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      AErnte2.3
  })
  
  output$Weinernte2.4 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland3.2.3 | Bundesland == input$Bundesland3.2.4) %>%
      filter(Messparameter == input$Messparameter3.2.2) %>%
      filter(Mostsorte == input$Mostsorte3.2)
  })
  
  ## Wetter 2.2 ----
  output$Wahl3.2.4 <- renderText({
    paste(input$Wetterphänomen3.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland3.2.5, "und", input$Bundesland3.2.6, ".")
  })
  
  output$Wetter2.3 <- renderPlot({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.5 | Bundesland == input$Bundesland3.2.6) %>%
      filter(Wetterphänomen == input$Wetterphänomen3.2) %>%
      ggplot()+
      aes(x = Jahr, y = Wert, color = Bundesland)+
      geom_point(size = 2)+
      geom_line(aes(group = Bundesland), size= 1.25)+
      scale_color_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 2160), breaks = seq(0, 2160, by = 100), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahr",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Deutscher Wetterdienst 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Wetter2.4 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Bundesland == input$Bundesland3.2.5 | Bundesland == input$Bundesland3.2.6) %>%
      filter(Wetterphänomen == input$Wetterphänomen3.2)
  })
  
  ## Weinernte 3.1 ----
  output$Wahl3.3.1 <- renderText({
    paste("Weinernte (", input$Messparameter3.3.1, ") der Bundesländer für die Mostsorte", input$Rebsorte3.3.1, "im Jahr", input$Jahr3.3.1, ".")
  })
  
  output$Weinernte3.1 <- renderPlot({
   Ernte3.1 <- E_BL_Jahr_RS_neu %>%
      filter(Mostsorte == input$Mostsorte3.3.1) %>%
      filter(Jahr == input$Jahr3.3.1) %>%
      filter(Messparameter == input$Messparameter3.3.1)
    
    AErnte3.1 <- NULL
    STErnte3.1 <- Ernte3.1 %>% 
      group_by(Bundesland) %>% 
      summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
      pull(Wert)
    if(STErnte3.1 == 0) {
      AErnte3.1 <- annotate("text", x = Bundesland, y = 6000000, label = "In diesem Bundesland gab es entweder keine Weinernte oder es liegen keine Daten vor.", size = 5)
    }
    Ernte3.1 %>%
      ggplot() +
      aes(x = Bundesland, y = Wert)+
      geom_col(position = "dodge", fill = "#2c3e50")+
      geom_label(aes(label=Wert)) +
      scale_y_continuous(limits = c(0, 12300970), breaks = seq(0, 12300970, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesland",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.202")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
    AErnte3.1
  })
  
  output$Weinernte3.2 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1)
  })
  
  ## Wetter 3.1 ----
  output$Wahl3.3.2 <- renderText({
    paste(input$Wetterphänomen3.3.1, "der Bundesländer im Jahr", input$Jahr3.3.2, ".")
  })
  
  output$Wetter3.1 <- renderPlot({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.1) %>%
      filter(Jahr == input$Jahr3.3.2) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert)+
      geom_col(position = "dodge", fill = "#2c3e50")+
      geom_label(aes(label=Wert)) +
      scale_y_continuous(limits = c(0, 2160), breaks = seq(0, 2160, by = 100), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesland",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Deutscher Wetterdienst 2020 | Stand: 18.08.2020")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Wetter3.2 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.1) %>%
      filter(Jahr == input$Jahr3.3.2)
  })
  
  ## Weinernte 3.2 ----
  output$Wahl3.3.3 <- renderText({
    paste("Weinernte (", input$Messparameter3.3.2, ") der Bundesländer für  die Mostsorte", input$Mostsorte3.3.2, "im Vergleich der Jahre", input$Jahr3.3.3, "und", input$Jahr3.3.4, ".")
  })
  
  output$Weinernte3.3 <- renderPlot({
    Ernte3.3 <- E_BL_Jahr_RS_neu %>%
      filter(Messparameter == input$Messparameter3.3.2) %>%
      filter(Mostsorte == input$Mostsorte3.3.2) %>%
      filter(Jahr == input$Jahr3.3.3 | Jahr == input$Jahr3.3.4) 
    
    AErnte3.3 <- NULL
    STErnte3.3 <- Ernte3.3 %>% 
      group_by(Bundesland) %>% 
      summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
      pull(Wert)
    if(STErnte3.3 == 0) {
      AErnte3.3 <- annotate("text", x = 2, y = 6000000, label = "In diesem Bundesland gab es entweder keine Weinernte oder es liegen keine Daten vor.", size = 5)
    }
    Ernte3.3 %>%
      ggplot()+
      aes(x = Bundesland, y = Wert)+
      geom_col(aes(fill = as.factor(Jahr)), position = "dodge")+
      geom_label(aes(label=Wert, group = Jahr), position = position_dodge(1), size = 3) +
      scale_fill_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 12300970), breaks = seq(0, 12300970, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesland",
        y = "Wert der Messvariablen",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.202",
        fill = "Jahr")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position="top") +
    AErnte3.3
  })
  
  output$Weinernte3.4 <- DT::renderDT({
    E_BL_Jahr_RS_neu %>%
      filter(Messparameter == input$Messparameter3.3.2) %>%
      filter(Mostsorte == input$Mostsorte3.3.2) %>%
      filter(Jahr == input$Jahr3.3.3 | Jahr == input$Jahr3.3.4)
  })
  
  ## Wetter 3.2 ----
  output$Wahl3.3.4 <- renderText({
    paste(input$Wetterphänomen3.3.2, "der Bundesländer im Vergleich der Jahre", input$Jahr3.3.5, "und", input$Jahr3.3.6, ".")
  })
  
  output$Wetter3.3 <- renderPlot({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.2) %>%
      filter(Jahr == input$Jahr3.3.5 | Jahr == input$Jahr3.3.6) %>%
      ggplot()+
      aes(x = Bundesland, y = Wert)+
      geom_col(aes(fill = as.factor(Jahr)), position = "dodge")+
      geom_label(aes(label=Wert, group = Jahr), position = position_dodge(1), size = 3) +
      scale_fill_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 2160), breaks = seq(0, 2160, by = 100), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesland",
        y = "Wert der Wettervariablen",
        caption = "Quelle & Copyright: Deutscher Wetterdienst 2020 | Stand: 18.08.2020",
        fill = "Jahr")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position="top")
  })
  
  output$Wetter3.4 <- DT::renderDT({
    Wetter_gesamt %>%
      filter(Wetterphänomen == input$Wetterphänomen3.3.2) %>%
      filter(Jahr == input$Jahr3.3.5 | Jahr == input$Jahr3.3.6)
  })
  
  
  # tabPanel 4 - Weinproduktion ----
  ## Produktion 1 ----
  output$Wahl4.1 <- renderText({
    paste("Weinbestand (in hl) nach Weinkategorie für", input$Bundesland4.1, "im Jahr", input$Jahr4.1, ".")
  })
  
  output$Weinproduktion1.1 <- renderPlot(width = "auto", height = 600, {
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.1) %>%
      filter(Jahr == input$Jahr4.1) %>%
      ggplot() +
      aes(x = Weinkategorie, y = hl) +
      geom_col(position = "dodge", fill = c("Weisswein: Qualitätswein" = "#8aa4be", "Weisswein: Prädikatswein" = "#8aa4be", "Weisswein: Wein und/oder Landwein" = "#8aa4be", "Weisswein: Insgesamt" = "#8aa4be", "Rotwein: Qualitätswein" = "#9e0657", "Rotwein: Prädikatswein" = "#9e0657", "Rotwein: Wein und/oder Landwein" = "#9e0657", "Rotwein: Insgesamt" = "#9e0657", "Insgesamt: Qualitätswein" = "#2c3e50", "Insgesamt: Prädikatswein" = "#2c3e50", "Insgesamt: Wein und/oder Landwein" = "#2c3e50", "Insgesamt: alle Rebsorten und Weinkategorien" = "#2c3e50")) +
      geom_label(aes(label=hl)) + 
      scale_y_continuous(limits = c(0, 10300000), breaks = seq(0, 10300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Weinkategorien",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion1.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.1) %>%
      filter(Jahr == input$Jahr4.1)
  })
  
  ## Produktion 2.1 ----
  output$Wahl4.2.1 <- renderText({
    paste("Weinbestand (in hl) nach Weinkategorie für", input$Bundesland4.2.1, "zwischen 2010 und 2018.")
  })
  
  output$Weinproduktion2.1 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.1) %>%
      filter(Weinkategorie %in% input$Weinkategorie) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Weinkategorie)+
      geom_point(size = 2)+
      geom_line(aes(group = Weinkategorie),size = 1.25)+
      scale_color_manual(values = c("Weißwein: Qualitätswein" = "#a6c3e3", "Weißwein: Prädikatswein" = "#8ca5bf", "Weißwein: Wein und/oder Landwein" = "#badbff", "Weißwein: Insgesamt" = "#66798c", "Rotwein: Qualitätswein" = "#d10873", "Rotwein: Prädikatswein" = "#9e0657", "Rotwein: Wein und/oder Landwein" = "#ff0a8d", "Rotwein: Insgesamt" = "#6b043b", "Insgesamt: Qualitätswein" = "#466482", "Insgesamt: Prädikatswein" = "#2c3e50", "Insgesamt: Wein und/oder Landwein" = "#628bb5", "Insgesamt: alle Rebsorten und Weinkategorien" = "#0F161C")) +
      scale_y_continuous(limits = c(0, 10300000), breaks = seq(0, 10300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion2.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.1)
  })
  
  ## Produktion 2.2 ----
  output$Wahl4.2.2 <- renderText({
    paste("Weinbestand (in hl) der Weinkategorie", input$Weinkategorie4.2, "zwischen 2010 und 2018 im Vergleich von", input$Bundesland4.2.2, "und", input$Bundesland4.2.3, ".")
  })
  
  output$Weinproduktion2.3 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.2 | Bundesland == input$Bundesland4.2.3) %>%
      filter(Weinkategorie == input$Weinkategorie4.2) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Bundesland)+
      geom_point(size = 2)+
      geom_line(aes(group = Bundesland), size = 1.25)+
      scale_color_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 10300000), breaks = seq(0, 10300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinproduktion2.4 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Bundesland == input$Bundesland4.2.2 | Bundesland == input$Bundesland4.2.3) %>%
      filter(Weinkategorie == input$Weinkategorie4.2)
  })
  
  ## Produktion 3.1 ----
  output$Wahl4.3.1 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Weinkategorie", input$Weinkategorie4.3.1, "im Jahr", input$Jahr4.3.1, ".")
  })
  
  output$Weinproduktion3.1 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.1) %>%
      filter(Jahr == input$Jahr4.3.1) %>%
      ggplot()+
      aes(x = Bundesland, y = hl)+
      geom_col(position = "dodge", fill = "#2c3e50")+
      geom_label(aes(label=hl)) + 
      scale_y_continuous(limits = c(0, 10300000), breaks = seq(0, 10300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesländer",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinproduktion3.2 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.1) %>%
      filter(Jahr == input$Jahr4.3.1)
  })
  
  ## Produktion 3.2 ----
  output$Wahl4.3.2 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Weinkategorie", input$Weinkategorie4.3.2, "im Vergleich der Jahre", input$Jahr4.3.2, "und", input$Jahr4.3.3, ".")
  })
  
  output$Weinproduktion3.3 <- renderPlot({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.2) %>%
      filter(Jahr == input$Jahr4.3.2 | Jahr == input$Jahr4.3.3) %>%
      ggplot()+
      aes(x = Bundesland, y = hl)+
      geom_col(position = "dodge", aes(fill = as.factor(Jahr)))+
      geom_label(aes(label=hl, group = Jahr), position = position_dodge(1), size = 2.25) +
      scale_fill_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 10300000), breaks = seq(0, 10300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesländer",
        y = "Weinproduktion in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020",
        fill = "Jahr")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position="top")
  })
  
  output$Weinproduktion3.4 <- DT::renderDT({
    WP_BL_Jahr_WK_neu %>%
      filter(Weinkategorie == input$Weinkategorie4.3.2) %>%
      filter(Jahr == input$Jahr4.3.2 | Jahr == input$Jahr4.3.3)
  })
  
  
  
  
  # tabPanel 5 - Weinbestände ----
  ## Bestand 1 ----
  output$Wahl5.1 <- renderText({
    paste("Weinbestand (in hl) nach Rebsorten für", input$Bundesland5.1, "im Jahr", input$Jahr5.1, ".")
  })
  
  output$Weinbestand1.1 <- renderPlot(width = "auto", height = 600, {
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.1) %>%
      filter(Jahr == input$Jahr5.1) %>% 
      ggplot() +
      aes(x = Rebsorte, y = hl) +
      geom_col(position = "dodge", fill = c("Weisswein" = "#8aa4be", "Rotwein" = "#9e0657", "Insgesamt" = "#2c3e50")) +
      geom_label(aes(label=hl)) + 
      scale_y_continuous(limits = c(0, 18300000), breaks = seq(0, 18300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Rebsorten",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
  })
  
  output$Weinbestand1.2 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.1) %>%
      filter(Jahr == input$Jahr5.1)
  })
  
  ## Bestand 2.1 ----
  output$Wahl5.2.1 <- renderText({
    paste("Weinbestand (in hl) nach Rebsorten für", input$Bundesland5.2.1, "zwischen 1993 und 2018.")
  })
  
  output$Weinbestand2.1 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.1) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Rebsorte)+
      geom_point(size = 2)+
      geom_line(aes(group = Rebsorte), size = 1.25)+
      scale_color_manual(values = c("Weisswein" = "#8aa4be", "Rotwein" = "#9e0657", "Insgesamt" = "#2c3e50")) +
      scale_y_continuous(limits = c(0, 18300000), breaks = seq(0, 18300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinbestand2.2 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.1)
  })
  
  ## Bestand 2.2 ----
  output$Wahl5.2.2 <- renderText({
    paste("Weinbestand (in hl) der Rebsorte", input$Rebsorte5.2, "zwischen 1993 und 2018 im Vergleich von", input$Bundesland5.2.2, "und", input$Bundesland5.2.3, ".")
  })
  
  output$Weinbestand2.3 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.2 | Bundesland == input$Bundesland5.2.3) %>%
      filter(Rebsorte == input$Rebsorte5.2) %>%
      ggplot()+
      aes(x = Jahr, y = hl, color = Bundesland)+
      geom_point(size = 2)+
      geom_line(aes(group = Bundesland), size = 1.25)+
      scale_color_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 18300000), breaks = seq(0, 18300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Jahre",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Weinbestand2.4 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Bundesland == input$Bundesland5.2.2 | Bundesland == input$Bundesland5.2.3) %>%
      filter(Rebsorte == input$Rebsorte5.2)
  })
  
  ## Bestand 3.1 ----
  output$Wahl5.3.1 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für die Rebsorte", input$Rebsorte5.3.1, "im Jahr", input$Jahr5.3.1, ".")
  })
  
  output$Weinbestand3.1 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1) %>%
      ggplot()+
      aes(x = Bundesland, y = hl)+
      geom_col(position = "dodge", fill = "#2c3e50")+
      geom_label(aes(label=hl)) + 
      scale_y_continuous(limits = c(0, 18300000), breaks = seq(0, 18300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesländer",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$Weinbestand3.2 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.1) %>%
      filter(Jahr == input$Jahr5.3.1)
  })
  
  ## Bestand 3.2 ----
  output$Wahl5.3.2 <- renderText({
    paste("Weinbestand (in hl) der Bundesländer für  die Rebsorte", input$Rebsorte5.3.2, "im Vergleich der Jahre", input$Jahr5.3.2, "und", input$Jahr5.3.3, ".")
  })
  
  output$Weinbestand3.3 <- renderPlot({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.2) %>%
      filter(Jahr == input$Jahr5.3.2 | Jahr == input$Jahr5.3.3) %>%
      ggplot()+
      aes(x = Bundesland, y = hl)+
      geom_col(position = "dodge", aes(fill = Jahr))+
      geom_label(aes(label=hl, group = Jahr), position = position_dodge(1), size = 2.25) +
      scale_fill_manual(values = c("#36587d", "#6DB2FC")) +
      scale_y_continuous(limits = c(0, 18300000), breaks = seq(0, 18300000, by = 1000000), labels = function(x) format(x, scientific = FALSE)) +
      labs(
        x = "Bundesländer",
        y = "Weinbestand in hl",
        caption = "Quelle & Copyright: Statistisches Bundesamt (Destatis), 2020 | Stand: 18.08.2020")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position="top")
  })
  
  output$Weinbestand3.4 <- DT::renderDT({
    WB_BL_Jahr_RS_neu %>%
      filter(Rebsorte == input$Rebsorte5.3.2) %>%
      filter(Jahr == input$Jahr5.3.2 | Jahr == input$Jahr5.3.3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)