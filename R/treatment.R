library(data.table)
library(dplyr)
library(sf)

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))                                                                                                                                                                                                                                                     
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}

# baixar dados
# system("cd data-raw && wget https://data.brasil.io/dataset/covid19/microdados_vacinacao.csv.gz")
# "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/PNI/vacina/2021/part-00000-e00e85d2-6871-4170-a5d7-74e1d84cfada-c000.csv"


dados <- fread("../data-raw/microdados_vacinacao.csv.gz")
# colnames(dados)

# download munis ------------

pais_sf <- geobr::read_country() %>% st_transform(4326)
munis_sf <- geobr::read_municipality() %>% st_transform(4326)
estados_sf <- geobr::read_state() %>% st_transform(4326)

# save it
readr::write_rds(pais_sf, "data/pais_sf.rds")
readr::write_rds(munis_sf, "data/munis_sf.rds")
readr::write_rds(estados_sf, "data/estados_sf.rds")


# calcular o total de pessoas que ja receberam vacina --------------------------

totais_pais <- dados %>%
  filter(numero_dose == 1) %>%
  mutate(id = "Brasil") %>%
  count(id) %>%
  setDT()


totais_estados <- dados %>%
  filter(numero_dose == 1) %>%
  count(estabelecimento_unidade_federativa) %>%
  setDT()


totais_munis <- dados %>%
  filter(numero_dose == 1) %>%
  group_by(estabelecimento_codigo_ibge_municipio) %>%
  summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>%
  setDT()



readr::write_rds(totais_pais, "data/totais_pais.rds")
readr::write_rds(totais_estados, "data/totais_estados.rds")
readr::write_rds(totais_munis, "data/totais_munis.rds")

# by age group ---------------------------
vacina_idade <- dados %>%
  count(estabelecimento_municipio_codigo, paciente_idade)

# by subgrupo
vacina_grupo_pais <- dados %>%
  group_by(paciente_grupo) %>%
  summarise(n = n()) %>% 
  mutate(id = "") %>%
  ungroup() %>% setDT()

vacina_grupo_estados <- dados %>%
  group_by(estabelecimento_unidade_federativa, paciente_grupo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(id = estabelecimento_unidade_federativa) %>%
  setDT()

vacina_grupo_munis <- dados %>%
  group_by(estabelecimento_codigo_ibge_municipio, paciente_grupo) %>%
  summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>%
  ungroup() %>% 
  mutate(id = estabelecimento_codigo_ibge_municipio) %>%
  setDT()

readr::write_rds(vacina_grupo_pais, "data/vacina_por_grupo_pais.rds")
readr::write_rds(vacina_grupo_estados, "data/vacina_por_grupo_estados.rds")
readr::write_rds(vacina_grupo_munis, "data/vacina_por_grupo_munis.rds")


# by data ---------------------------------
vacina_dia_pais <- dados %>%
  mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
  # tirar tudo antes do dia 1701 (primeiro dia de fato)
  filter(data_aplicacao >= as.Date("2021-01-17")) %>%
  group_by(data_aplicacao) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(id = "") %>%
  setDT()

# calculate rolling seven day average
vacina_dia_pais[, n_7mean := frollmean(n, 7)]
vacina_dia_pais[, n_7mean := round(n_7mean, 0)]

vacina_dia_estados <- dados %>%
  mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
  filter(data_aplicacao >= as.Date("2021-01-17")) %>%
  group_by(estabelecimento_unidade_federativa, data_aplicacao) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(id = estabelecimento_unidade_federativa) %>%
  setDT()

# calculate rolling seven day average
vacina_dia_estados[, n_7mean := frollmean(n, 7), 
            by = estabelecimento_unidade_federativa]
vacina_dia_estados[, n_7mean := round(n_7mean, 0)]

vacina_dia_munis <- dados %>%
  mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
  filter(data_aplicacao >= as.Date("2021-01-17")) %>%
  group_by(estabelecimento_codigo_ibge_municipio, data_aplicacao) %>%
  summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>% 
  ungroup() %>%
  mutate(id = estabelecimento_codigo_ibge_municipio) %>%
  setDT()

# calculate rolling seven day average
vacina_dia_munis[, n_7mean := frollmean(n, 7), 
                 by = estabelecimento_codigo_ibge_municipio]
vacina_dia_munis[, n_7mean := round(n_7mean, 0)]



# save it
readr::write_rds(vacina_dia_pais, "data/vacina_dia_pais.rds")
readr::write_rds(vacina_dia_estados, "data/vacina_dia_estados.rds")
readr::write_rds(vacina_dia_munis, "data/vacina_dia_munis.rds")

# table(dados$paciente_grupo)  
# table(dados$paciente_subgrupo)  

# count(dados, estabelecimento_codigo_cnes, sort = TRUE)

fread("data-raw/part-00000-e00e85d2-6871-4170-a5d7-74e1d84cfada-c000.csv") %>% nrow()
