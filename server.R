library(dplyr)
library(data.table)
library(shinyWidgets)
library(shinyBS)
library(highcharter)
library(shinyjs)
library(lubridate)
library(readr)
library(sf)
library(leaflet)
library(scales)
library(shiny)
library(shinydashboard)

# geo data


pais <- readr::read_rds("data/pais_sf.rds")
munis <- readr::read_rds("data/munis_sf.rds")
estados <- readr::read_rds("data/estados_sf.rds")

# abrir dados de vacinacao ----------
vacina_grupo_pais <- read_rds("data/vacina_por_grupo_pais.rds")
vacina_grupo_estados <- read_rds("data/vacina_por_grupo_estados.rds")
vacina_grupo_munis <- read_rds("data/vacina_por_grupo_munis.rds")

vacina_dia_pais <- read_rds("data/vacina_dia_pais.rds")
vacina_dia_estados <- read_rds("data/vacina_dia_estados.rds")
vacina_dia_munis <- read_rds("data/vacina_dia_munis.rds")


totais_pais <- read_rds("data/totais_pais.rds")
totais_estados <- read_rds("data/totais_estados.rds")
totais_munis <- read_rds("data/totais_munis.rds")







server <- function(input, output, session) {
  
  # estabelecer reactive para escolher a base de dados a ser utilizada
  # a depender de cada escolha
  data <- reactiveValues()
  
  # quando acontecer alguma mudanca no estado:
  observeEvent({input$estado},{ 
    
    
    # se o estado for vazio, escolher os dados do brasil
    if(input$estado == "") {
      
      data$grupo <- vacina_grupo_pais
      data$dia <- vacina_dia_pais
      data$totais <- totais_pais
      data$mapa <- pais
      
    }
    
    # se o estado nao for vazio, escolher os dados do estado
    else {
      
      # # quando escolher estado, zerar o municipio
      # input$cidade <- NULL
      
      data$grupo <- vacina_grupo_estados[estabelecimento_unidade_federativa == input$estado]
      data$dia <- vacina_dia_estados[estabelecimento_unidade_federativa == input$estado]
      data$totais <- totais_estados[estabelecimento_unidade_federativa == input$estado]
      data$mapa <- estados %>% filter(abbrev_state == input$estado)
      
      
      
    }
    
    # print(sprintf("Estado: %s", input$estado))
    # print(sprintf("Cidade: %s", input$cidade))
    
  })
  
  
  # quando acontecer alguma mudanca no municipio:
  observeEvent({input$cidade},{
    
    if(input$cidade != "") {
      
      data$grupo <- vacina_grupo_munis[uf == input$estado & estabelecimento_codigo_ibge_municipio == input$cidade]
      data$dia <- vacina_dia_munis[uf == input$estado & estabelecimento_codigo_ibge_municipio == input$cidade]
      data$totais <- totais_munis[uf == input$estado & estabelecimento_codigo_ibge_municipio == input$cidade]
      data$mapa <- munis %>% filter(abbrev_state == input$estado & code_muni == input$cidade)
      
      # print(v_estado$estado)
      
      # print(input$cidade)
      
    }
    
  })
  
  
  
  # grafico
  
  output$graph_dia <- renderHighchart({
    
    # req(input$estado)
    # req(input$cidade)
    
    # print(sprintf("Estado: %s", input$estado))
    # print(head(data$mapa))
    print(head(data$dia))
    # print(input$cidade)
    
    data$dia[, data_aplicacao := datetime_to_timestamp(data_aplicacao)]
    
    highchart() %>%
      hc_xAxis(categories = data$dia$data_aplicacao,
               type = 'datetime',
               # ticks every month
               # tickInterval =  1000 * 3600 * 24 * 50,
               labels =
                 list(enabled = TRUE,
                      format = "{value:%b-%d}",
                      # labels every 15 days
                      step = 15)
      ) %>%
      hc_add_series(data = data$dia$n,
                    type = "column",
                    name = "Total do dia") %>%
      hc_add_series(data = data$dia$n_7mean, 
                    type = "line", 
                    name = "Média móvel de 7 dias") %>%
      hc_yAxis(labels = list(enabled = TRUE,
                             format = "{value:,.0f}"),
               title = list(text = "Número de vacinados"),
               tickLength = 0,
               gridLineWidth = 0) %>%
      hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
      hc_plotOptions(column = list(borderRadius = 1,
                                   borderColor = "#000000",
                                   color = "#F4F4F4",
                                   # tooltip = list(
                                   # pointFormat = sprintf("%s: {point.y}", "Passageiros"),
                                   # valueDecimals = 0),
                                   # stacking = FALSE,
                                   # events = list(click = ClickFunction),
                                   allowPointSelect = TRUE
                                   
                                   
      ))
    
    
  })
  
  
  
  output$graph_grupo <- renderHighchart({
    
    
    hchart(data$grupo, "bar", hcaes(y = n, x = paciente_grupo),
           name = "Frequência") %>%
      hc_yAxis(labels = list(enabled = FALSE),
               title = list(text = ""),
               tickLength = 0,
               gridLineWidth = 0)
    
    
  })
  
  
  # RENDER UI -----------------------------------------------------------------------------------
  
  cidades_choices <- reactive({
    
    if(input$estado != "") {
      
      # print(input$estado)
      a <- munis %>% filter(abbrev_state == input$estado)
      
      munis_go <- as.character(a$code_muni)
      names(munis_go) <- a$name_muni
      
      return(munis_go)
      
    }
    
  })
  
  
  output$escolher_cidade <- renderUI({
    
    
    conditionalPanel(
      condition = "input.estado != ''",
      pickerInput(inputId = "cidade", 
                  label = "Cidade: ", 
                  selected = NULL,
                  choices = cidades_choices(),
                  options = list(
                    `live-search` = TRUE,
                    title = "Selecione a cidade")
      )
    )
    
    
    
  })
  
  
  
  
  # RENDER BRAZIL'S BASEMAP -------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(-43.95988, -19.902739, zoom = 3)
    
    
  })
  
  
  
  
  observeEvent({c(input$estado, input$cidade)},{
    
    # print(centroid_go()$lon)
    # print(centroid_go()$lat)
    
    # if(input$estado != "") {
    
    bounds_map <- st_bbox(data$mapa) 
    names(bounds_map) <- NULL
    
    leafletProxy(mapId = "map", data = data$mapa) %>%
      # leaflet::clearBounds() %>%
      # leaflet::clearControls() %>%
      leaflet::clearShapes() %>%
      addPolygons(stroke = TRUE, fillOpacity = 0) %>%
      flyToBounds(lng1 = bounds_map[1], lat1 = bounds_map[2],
                  lng2 = bounds_map[3], lat2 = bounds_map[4],
                  options = list(animate = TRUE)) 
    # }
    
  })
  
  
  
  
  # infobox -----------------------------------------------------------------
  
  
  output$total_doses <- renderInfoBox({
    infoBox(
      title = "Doses aplicadas", 
      fill = FALSE,
      value =  sprintf("%s doses", scales::comma(sum(data$dia$n), accuracy = 1, scale = 1, big.mark = "\\.")),
      # value =  HTML(route_df()$od),
      icon = icon("syringe"),
      color = "black"
    )
  })
  
  output$total_vacinados <- renderInfoBox({
    infoBox(
      title = "Pessoas vacinadas", 
      fill = FALSE,
      value =  sprintf("%s pessoas", scales::comma(data$totais$n, accuracy = 1, scale = 1, big.mark = "\\.")),
      # value =  HTML(route_df()$od),
      icon = icon("user-friends"),
      color = "black"
    )
  })
  
  
}

