library(data.table)
library(dplyr)
library(sf)
library(readr)

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

# 0) baixar dados ------------------------------
curl::curl_download("https://data.brasil.io/dataset/covid19/microdados_vacinacao.csv.gz",
                    destfile = "../../data-raw/painel_vacinacao_covid/microdados_vacinacao.csv.gz",
                    quiet = FALSE)
# system("cd data-raw && wget https://data.brasil.io/dataset/covid19/microdados_vacinacao.csv.gz")
# "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/PNI/vacina/2021/part-00000-e00e85d2-6871-4170-a5d7-74e1d84cfada-c000.csv"




# 1) abrir dados e salvar como data.table descompacatado --------------------------------------------------

dados <- fread("../../data-raw/painel_vacinacao_covid/microdados_vacinacao.csv.gz")
dados[, estabelecimento_codigo_cnes := stringr::str_pad(estabelecimento_codigo_cnes, width = 7, pad = 0)]

# extrair data de atualizacao
data_atualizacao <- max(dados$data_aplicacao) %>% write_rds("data/data_atualizacao.rds")

fwrite(dados, "../../data/painel_vacinacao_covid/microdados_vacinacao.csv")
# colnames(dados)

dados <- fread("../../data/painel_vacinacao_covid/microdados_vacinacao.csv",
               colClasses = 'character')

# 2) download shapes ------------

pais_sf <- geobr::read_country() %>% st_transform(4326)
munis_sf <- geobr::read_municipality() %>% st_transform(4326)
estados_sf <- geobr::read_state() %>% st_transform(4326)

# save it
readr::write_rds(pais_sf, "data/pais_sf.rds")
readr::write_rds(munis_sf, "data/munis_sf.rds")
readr::write_rds(estados_sf, "data/estados_sf.rds")


# 3) calcular o total de pessoas que ja receberam vacina --------------------------

totais_pais <- dados %>%
  filter(numero_dose == 1) %>%
  mutate(id = "") %>%
  count(id) %>%
  setDT()


totais_estados <- dados %>%
  filter(numero_dose == 1) %>%
  mutate(id = estabelecimento_unidade_federativa) %>%
  count(estabelecimento_unidade_federativa, id) %>%
  setDT()


totais_munis <- dados %>%
  filter(numero_dose == 1) %>%
  mutate(id = estabelecimento_codigo_ibge_municipio) %>%
  group_by(estabelecimento_codigo_ibge_municipio, id) %>%
  summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>%
  setDT()



readr::write_rds(totais_pais, "data/totais_pais.rds")
readr::write_rds(totais_estados, "data/totais_estados.rds")
readr::write_rds(totais_munis, "data/totais_munis.rds")

# 4) totais by age group ---------------------------

# reclassificar grupo nao classificado
table(dados$paciente_grupo, useNA = 'always')
dados[, paciente_grupo := fifelse(paciente_grupo == "", "Sem classificação", paciente_grupo)]


# by subgrupo

vacina_grupo_pais <- dados[, .(n = .N), 
                           by = paciente_grupo]
vacina_grupo_pais[, id := ""]

# vacina_grupo_pais <- dados %>%
#   group_by(paciente_grupo) %>%
#   summarise(n = n()) %>% 
#   mutate(id = "") %>%
#   ungroup() %>% setDT()

vacina_grupo_estados <- dados[, .(n = .N), 
                              by = .(estabelecimento_unidade_federativa, paciente_grupo)]
vacina_grupo_estados[, id := estabelecimento_unidade_federativa]

# vacina_grupo_estados <- dados %>%
#   group_by(estabelecimento_unidade_federativa, paciente_grupo) %>%
#   summarise(n = n()) %>%
#   ungroup() %>% 
#   mutate(id = estabelecimento_unidade_federativa) %>%
#   setDT()


vacina_grupo_munis <- dados[, .(n = .N, uf = estabelecimento_unidade_federativa[1]), 
                            by = .(estabelecimento_codigo_ibge_municipio, paciente_grupo)]
vacina_grupo_munis[, id := estabelecimento_codigo_ibge_municipio]

# vacina_grupo_munis <- dados %>%
#   group_by(estabelecimento_codigo_ibge_municipio, paciente_grupo) %>%
#   summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>%
#   ungroup() %>% 
#   mutate(id = ) %>%
#   setDT()

readr::write_rds(vacina_grupo_pais, "data/vacina_por_grupo_pais.rds")
readr::write_rds(vacina_grupo_estados, "data/vacina_por_grupo_estados.rds")
readr::write_rds(vacina_grupo_munis, "data/vacina_por_grupo_munis.rds")


# 5) totais por dia ---------------------------------
dados[, data_aplicacao := as.Date(data_aplicacao)]
# tirar tudo antes do dia 1701 (primeiro dia de fato)
vacina_dia_pais <- dados[data_aplicacao >= as.Date("2021-01-17")]
vacina_dia_pais <- vacina_dia_pais[, .(n = .N), 
                                   by = .(data_aplicacao) ]
vacina_dia_pais[, id := ""]

# vacina_dia_pais <- dados %>%
#   mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
#   # tirar tudo antes do dia 1701 (primeiro dia de fato)
#   filter(data_aplicacao >= as.Date("2021-01-17")) %>%
#   group_by(data_aplicacao) %>%
#   summarise(n = n()) %>% 
#   ungroup() %>%
#   mutate(id = "Brasil") %>%
#   setDT()

# calculate rolling seven day average
vacina_dia_pais[, n_7mean := frollmean(n, 7)]
vacina_dia_pais[, n_7mean := round(n_7mean, 0)]

# tirar tudo antes do dia 1701 (primeiro dia de fato)
vacina_dia_estados <- dados[data_aplicacao >= as.Date("2021-01-17")]
vacina_dia_estados <- vacina_dia_estados[, .(n = .N), 
                                   by = .(estabelecimento_unidade_federativa, data_aplicacao) ]
vacina_dia_estados[, id := estabelecimento_unidade_federativa]

# vacina_dia_estados <- dados %>%
#   mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
#   filter(data_aplicacao >= as.Date("2021-01-17")) %>%
#   group_by(estabelecimento_unidade_federativa, data_aplicacao) %>%
#   summarise(n = n()) %>% 
#   ungroup() %>%
#   mutate(id = estabelecimento_unidade_federativa) %>%
#   setDT()

# calculate rolling seven day average
vacina_dia_estados[, n_7mean := frollmean(n, 7), 
                   by = estabelecimento_unidade_federativa]
vacina_dia_estados[, n_7mean := round(n_7mean, 0)]


# tirar tudo antes do dia 1701 (primeiro dia de fato)
vacina_dia_munis <- dados[data_aplicacao >= as.Date("2021-01-17")]
vacina_dia_munis <- vacina_dia_munis[, .(n = .N, uf = estabelecimento_unidade_federativa[1]), 
                                         by = .(estabelecimento_codigo_ibge_municipio, data_aplicacao) ]
vacina_dia_munis[, id := estabelecimento_codigo_ibge_municipio]

# vacina_dia_munis <- dados %>%
#   mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
#   filter(data_aplicacao >= as.Date("2021-01-17")) %>%
#   group_by(estabelecimento_codigo_ibge_municipio, data_aplicacao) %>%
#   summarise(n = n(), uf = first(estabelecimento_unidade_federativa)) %>% 
#   ungroup() %>%
#   mutate(id = estabelecimento_codigo_ibge_municipio) %>%
#   setDT()

# calculate rolling seven day average
vacina_dia_munis[, n_7mean := frollmean(n, 7), 
                 by = estabelecimento_codigo_ibge_municipio]
vacina_dia_munis[, n_7mean := round(n_7mean, 0)]



# save it
readr::write_rds(vacina_dia_pais, "data/vacina_dia_pais.rds")
readr::write_rds(vacina_dia_estados, "data/vacina_dia_estados.rds")
readr::write_rds(vacina_dia_munis, "data/vacina_dia_munis.rds")



# 6) localizacao dos postos de vaciancao ------------------------------------------------------

# abrir a localizacao dos postos
localizacao_postos <- readr::read_rds("../../data/painel_vacinacao_covid/geocode/geocode_vacinacao_google2.rds")
# filtrar somente os com boa qualidade
localizacao_postos <- localizacao_postos[PrecisionDepth %in% c("street_number", "route", "airport", 
                                                               "amusement_park","bus_station","establishment",
                                                               "intersection",
                                                               "neighborhood",
                                                               "political",
                                                               "post_box",
                                                               "premise",
                                                               "subpremise",
                                                               "postal_code",
                                                               "town_square")]

# filtra somente os que estao no brasil
localizacao_postos <- localizacao_postos %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(readr::read_rds("data/pais_sf.rds") %>% mutate(a = 1)) %>%
  filter(!is.na(a)) %>%
  select(-a) %>%
  sfc_as_cols() %>%
  setDT()

# library(mapview)
# localizacao_postos %>% filter(!is.na(lon)) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()

localizacao_postos <- localizacao_postos[, .(estabelecimento_codigo_cnes, lon, lat)]

# calcular doses por postos
dados_por_postos <- dados[, .(doses_n = .N), 
                          by = .(estabelecimento_codigo_cnes, 
                                 estabelecimento_codigo_ibge_municipio,
                                 estabelecimento_unidade_federativa,
                                 estabelecimento)]

# juntar as localizacoes com as dodese
dados_por_postos_coords <- merge(
  dados_por_postos,
  localizacao_postos,
  by = "estabelecimento_codigo_cnes",
  all.x = TRUE
)


dados_por_postos_coords_sf <- dados_por_postos_coords %>% 
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

# salvar como sf
readr::write_rds(dados_por_postos_coords_sf, "data/postos_n_coords.rds")



# calcular quantos dias para terminar vacinacao -----------------------------------------------

# para o brasil ----------------
vacina_dia_pais <- readr::read_rds("data/vacina_dia_pais.rds") %>%
  # calcular o total ate agora
  arrange(desc(data_aplicacao)) %>%
  group_by(id) %>%
  summarise(ritmo = first(n_7mean),
            total = sum(n))

# calcular a quantidade que ainda falta
# abrir estimativas
estimativas_pop <- read_rds("../../data/painel_vacinacao_covid/estimativas_pop_ibge_2020.rds") %>%
  mutate(id = "") %>%
  group_by(id) %>%
  summarise(total = sum(populacao_estimada)) %>%
  # calcular 70% disso
  mutate(total_70 = as.integer(0.7 * total)) %>%
  # doses necessarias para toda populacao
  mutate(doses_necessarias = total_70 * 2) %>%
  select(id, doses_necessarias)

# juntar com a base do brasil
vacina_dia_pais <- vacina_dia_pais %>%
  left_join(estimativas_pop, by = "id") %>%
  mutate(dias_faltantes = as.integer((doses_necessarias - total)/ritmo))


# para os estados ----------------
vacina_dia_estados <- readr::read_rds("data/vacina_dia_estados.rds") %>%
  # calcular o total ate agora
  arrange(desc(data_aplicacao)) %>%
  group_by(id) %>%
  summarise(ritmo = first(n_7mean),
            total = sum(n))

# calcular a quantidade que ainda falta
# abrir estimativas
estimativas_pop <- read_rds("../../data/painel_vacinacao_covid/estimativas_pop_ibge_2020.rds") %>%
  mutate(id = uf) %>%
  group_by(id) %>%
  summarise(total = sum(populacao_estimada)) %>%
  # calcular 70% disso
  mutate(total_70 = as.integer(0.7 * total)) %>%
  # doses necessarias para toda populacao
  mutate(doses_necessarias = total_70 * 2) %>%
  select(id, doses_necessarias)

# juntar com a base do brasil
vacina_dia_estados <- vacina_dia_estados %>%
  left_join(estimativas_pop, by = "id") %>%
  mutate(dias_faltantes = as.integer((doses_necessarias - total)/ritmo))


# para os municipios ----------------
vacina_dia_munis <- readr::read_rds("data/vacina_dia_munis.rds") %>%
  # calcular o total ate agora
  arrange(desc(data_aplicacao)) %>%
  group_by(id) %>%
  summarise(ritmo = first(n_7mean),
            total = sum(n))

# calcular a quantidade que ainda falta
# abrir estimativas
estimativas_pop <- read_rds("../../data/painel_vacinacao_covid/estimativas_pop_ibge_2020.rds") %>%
  mutate(id = code_muni) %>%
  group_by(id) %>%
  summarise(total = sum(populacao_estimada)) %>%
  # calcular 70% disso
  mutate(total_70 = as.integer(0.7 * total)) %>%
  # doses necessarias para toda populacao
  mutate(doses_necessarias = total_70 * 2) %>%
  select(id, doses_necessarias)

# juntar com a base do brasil
vacina_dia_munis <- vacina_dia_munis %>%
  left_join(estimativas_pop, by = "id") %>%
  mutate(dias_faltantes = as.integer((doses_necessarias - total)/ritmo))
  

# trazer essa coluna para os totais
totais_pais <-    read_rds("data/totais_pais.rds") %>% left_join(vacina_dia_pais)
totais_estados <- read_rds("data/totais_estados.rds") %>% left_join(vacina_dia_estados)
totais_munis <-   read_rds("data/totais_munis.rds") %>% left_join(vacina_dia_munis)

# join


# save it
readr::write_rds(totais_pais,    "data/totais_pais.rds")
readr::write_rds(totais_estados, "data/totais_estados.rds")
readr::write_rds(totais_munis,   "data/totais_munis.rds")

