library(dplyr)
library(data.table)
library(shinyWidgets)
library(shinyBS)
library(ggplot2)
library(highcharter)
# library(shinyjs)
library(lubridate)
# library(readr)

library(shiny)
library(shinydashboard)
library(leaflet)

atualizacao <- readRDS("data/data_atualizacao.rds") %>% as.Date()
atualizacao <- format(atualizacao, "%d/%m/%y")

header <- dashboardHeader(
  title = "Painel da vacinação COVID-19",
  titleWidth = 400,
  disable = FALSE,
  tags$li(class="dropdown", a("Dados de vacinação: ", href="https://brasil.io/", "brasil.io", target="_blank")),
  # tags$li(class="dropdown", uiOutput("dica")),
  tags$li(class="dropdown", tags$a(href="https://data.brasil.io/dataset/covid19/microdados_vacinacao.csv.gz", sprintf("Última atualização: %s", atualizacao), target="_blank")),
  # tags$li(class="dropdown", tags$a(href="https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao", "Última atualização: 12/03/2021", target="_blank")),
  tags$li(class="dropdown", tags$a(href="https://kauebraga.dev/", "Contato", target="_blank")),
  # tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-itil%C2%AE-5720309/" ,icon("linkedin"), "My Profile", target="_blank")),
  tags$li(class="dropdown",tags$a(href="https://github.com/kauebraga/painel_vacinacao_covid", icon("github"), "Código", target="_blank"))
)

body <- dashboardBody(
  fluidRow(
    column(width = 5,
           box(width = NULL, 
               status = "primary",
               collapsible = FALSE,
               pickerInput(inputId = "estado", 
                           label = "Estado: ", 
                           options = list(title = "Selecione o estado"),
                           choices = list(
                             'Norte' = c('Rondônia' =	"RO",
                                         'Acre' =	"AC",
                                         'Amazonas' =	"AM",
                                         'Roraima' =	"RR",
                                         'Pará' =	"PA",
                                         'Amapá' =	"AP",
                                         'Tocantins' =	"TO"
                             ),
                             'Nordeste' = c('Maranhão' =	"MA",
                                            'Piauí' =	"PI",
                                            'Ceará' =	"CE",
                                            'Rio Grande do Norte' =	"RN",
                                            'Paraíba' =	"PB",
                                            'Pernambuco' =	"PE",
                                            'Alagoas' =	"AL",
                                            'Sergipe' =	"SE",
                                            'Bahia' =	"BA"),
                             'Sudeste' = c('Minas Gerais' =	"MG",
                                           'Espírito Santo' =	"ES",
                                           'Rio de Janeiro' =	"RJ",
                                           'São Paulo' =	"SP"),
                             'Sul' = c('Paraná' =	"PR",
                                       'Santa Catarina' =	"SC",
                                       'Rio Grande do Sul' =	"RS"),
                             'Centro-Oeste' = c('Mato Grosso do Sul' = "MS",
                                                'Mato Grosso' =	"MT",
                                                'Goiás'=	"GO",
                                                'Distrito Federal' =	"DF")
                           )),
               uiOutput("escolher_cidade")
               # conditionalPanel(condition = "input.map_arc_click, input.Clicked",
               
               
           ),
           box(width = NULL,
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               # title = "Por grupo",
               infoBoxOutput("total_doses", width = 6),
               infoBoxOutput("total_vacinados", width = 6),
               highchartOutput("graph_grupo"),
               infoBoxOutput("quanto_falta", width = 12))
           
    ),
    column(width = 7,
           box(width = NULL,
               # height = "100%",
               solidHeader = TRUE,
               # title = "Mapa",
               leafletOutput("map", height = "calc(100vh - 450px)")
               # mapdeckOutput("map", height = "calc(100vh - 400px)")
           ),
           box(width = NULL,
               # height = "100%",
               solidHeader = TRUE,
               # title = "Por dia",
               highchartOutput("graph_dia", height = "250px"))
    ),
    
),

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
),
tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.setInputValue(variableName, null);
    });
    ")
)


dashboardPage(
  title = "Painel da vacinação COVID-19",
  header,
  dashboardSidebar(disable = T),
  body,
  skin = "black"
)