library(dplyr)
library(data.table)
library(shinyWidgets)
library(shinyBS)
library(highcharter)
# library(shinyjs)
# library(lubridate)
# library(readr)
library(sf)
library(leaflet)
library(scales)
library(shiny)
library(shinydashboard)
# library(htmltools)

# set highchart options for portuguese
lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$months <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
lang$shortMonths <- c('Jan', 'Fev', 'Mar', 'Abr', 'Maio', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')
lang$weekdays <- c('Domingo', 'Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta', 'Sábado')
# lang$numericSymbols <- highcharter::JS("null") # optional: remove the SI prefixes
options(highcharter.lang = lang)

# geo data


pais <- readRDS("data/pais_sf.rds")
munis <- readRDS("data/munis_sf.rds")
estados <- readRDS("data/estados_sf.rds")

# abrir dados de vacinacao ----------
vacina_grupo_pais <- readRDS("data/vacina_por_grupo_pais.rds")
vacina_grupo_estados <- readRDS("data/vacina_por_grupo_estados.rds")
vacina_grupo_munis <- readRDS("data/vacina_por_grupo_munis.rds")

vacina_dia_pais <- readRDS("data/vacina_dia_pais.rds")
vacina_dia_estados <- readRDS("data/vacina_dia_estados.rds")
vacina_dia_munis <- readRDS("data/vacina_dia_munis.rds")


totais_pais <- readRDS("data/totais_pais.rds")
totais_estados <- readRDS("data/totais_estados.rds")
totais_munis <- readRDS("data/totais_munis.rds")

postos_n_coords <- readRDS("data/postos_n_coords.rds")





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
      data$postos <- postos_n_coords %>% filter(estabelecimento_unidade_federativa == input$estado)
      
      
      
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
      data$postos <- postos_n_coords %>% filter(estabelecimento_codigo_ibge_municipio == input$cidade)
      
      # print(v_estado$estado)
      
      # print(input$cidade)
      
      # print(head(data$postos))
      
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
    
    print(class(data$dia$data_aplicacao))
    
    # data$dia[, data_aplicacao := datetime_to_timestamp(data_aplicacao)]
    
    highchart() %>%
      hc_xAxis(categories = datetime_to_timestamp(data$dia$data_aplicacao),
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
                                   tooltip = list(
                                     xDateFormat = "%d-%m-%Y",
                                     pointFormat = sprintf("%s: {point.y}", "Total do dia"),
                                     valueDecimals = 0),
                                   # tooltip = list(
                                   # pointFormat = sprintf("%s: {point.y}", "Passageiros"),
                                   # valueDecimals = 0),
                                   # stacking = FALSE,
                                   # events = list(click = ClickFunction),
                                   allowPointSelect = TRUE),
                     line = list(color = "#000000",
                                 tooltip = list(
                                   xDateFormat = "%d-%m-%Y",
                                   pointFormat = sprintf("%s: {point.y}", "Média móvel"),
                                   valueDecimals = 0)
                     )
      )
    
    
  })
  
  
  
  output$graph_grupo <- renderHighchart({
    
    
    hchart(data$grupo, "bar", hcaes(y = n, x = paciente_grupo),
           name = "Frequência") %>%
      hc_xAxis(
        title = list(text = "")
      #   labels = list(useHTML = TRUE,
      #                 style = list(width = '100px')))%>%
      ) %>%
      hc_yAxis(labels = list(enabled = TRUE),
               title = list(text = ""),
               tickLength = 0,
               gridLineWidth = 0) %>%
      hc_plotOptions(bar = list(borderRadius = 1,
                                borderColor = "#000000",
                                color = "#F4F4F4",
                                tooltip = list(
                                  pointFormat = sprintf("%s: {point.y}", "Total"),
                                  valueDecimals = 0),
                                # stacking = FALSE,
                                allowPointSelect = TRUE
                                
                                
      ))
    
    
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
    
    # print(input$cidade)
    # print(input$estado)
    
    # if(input$estado != "") {
    
    bounds_map <- st_bbox(data$mapa) 
    names(bounds_map) <- NULL
    
    # color pallete for markers
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = data$postos$doses_n
    )
    
    # radius
    radius_size <- rescale(data$postos$doses_n)
    
    leaflet_base <- leafletProxy(mapId = "map", data = data$mapa) %>%
      # leaflet::clearBounds() %>%
      leaflet::clearMarkers() %>%
      leaflet::clearControls() %>%
      leaflet::clearShapes() %>%
      addPolygons(stroke = TRUE, fillOpacity = 0, color = 'black', opacity = 1,
                  weight = 1) %>%
      flyToBounds(lng1 = bounds_map[1], lat1 = bounds_map[2],
                  lng2 = bounds_map[3], lat2 = bounds_map[4],
                  options = list(animate = FALSE,
                                 duration = 0.8,
                                 easeLinearity = 0.1,
                                 noMoveStart = FALSE))
    
    
    if(input$estado != "") {
      
      # labels for the circles
      data$postos <- data$postos %>%
        mutate(label1 = sprintf('%s<br/><strong>Total de doses:</strong> %s', 
                                estabelecimento, 
                                doses_n))
      
      # make them HTML like
      labels_html <- lapply(data$postos$label1, HTML)
      
      leaflet_base %>%
        addCircleMarkers(data = data$postos,
                         radius = ~ rescale(data$postos$doses_n, c(4,10)),
                         # color = ~pal(doses_n),
                         stroke = FALSE, fillOpacity = 0.5,
                         fillColor = 'black',
                         label = labels_html)
      # addLegend("bottomleft", pal = pal, values = ~data$postos$doses_n,
      #           title = "Quantidade de doses aplicadas",
      #           # labFormat = labelFormat(prefix = "$"),
      #           opacity = 1)
    }
    
  })
  
  
  
  
  # infobox -----------------------------------------------------------------
  
  
  output$total_doses <- renderInfoBox({
    infoBox(
      title = "Doses", 
      fill = FALSE,
      value =  sprintf("%s doses", scales::comma(sum(data$dia$n), accuracy = 1, scale = 1, big.mark = "\\.")),
      # value =  HTML(route_df()$od),
      icon = icon("syringe"),
      color = "black"
    )
  })
  
  output$total_vacinados <- renderInfoBox({
    infoBox(
      title = "Pessoas", 
      fill = FALSE,
      value =  sprintf("%s pessoas", scales::comma(data$totais$n, accuracy = 1, scale = 1, big.mark = "\\.")),
      # value =  HTML(route_df()$od),
      icon = icon("user-friends"),
      color = "black"
    )
  })
  
  
}

