library(data.table)
library(dplyr)
library(ggmap)
library(Hmisc)

# openvacinacao
data_vacinacao <- fread("../data/painel_vacinacao_covid/microdados_vacinacao.csv",
                        select = c("estabelecimento_codigo_cnes", "estabelecimento_municipio",
                                   "estabelecimento_unidade_federativa", 
                                   "estabelecimento_razao_social",
                                   "estabelecimento"))

# abrir dados do cnes com o endereco dos locais
# baixar daqui: http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp
data_cnes <- fread("../data-raw/painel_vacinacao_covid/tbEstabelecimento202101.csv", colClasses = 'character',
                   select = c("CO_CNES", "NO_LOGRADOURO", "NU_ENDERECO", "NO_BAIRRO",  "CO_CEP"))


# testes
# fortaleza
a <- data_vacinacao %>% filter(estabelecimento_municipio == "Fortaleza") %>% distinct(estabelecimento_codigo_cnes, .keep_all = TRUE)
b <- data_cnes[CO_CNES == "9094857"]
# poucos digitos
a <- data_vacinacao[, nchar_cnes := nchar(estabelecimento_codigo_cnes)]
a1 <- a[nchar_cnes < 7]
a1 <- distinct(a1, estabelecimento_codigo_cnes, .keep_all = TRUE)

# verificar numero de chars no cnes
# table(nchar(data_vacinacao$estabelecimento_codigo_cnes))
# data_vacinacao %>% mutate(a = nchar(estabelecimento_codigo_cnes)) %>% filter(a != 7) %>% 
# distinct(estabelecimento_codigo_cnes, estabelecimento_razao_social) %>% View()

# table(nchar(data_cnes$CO_CNES))
data_vacinacao[, estabelecimento_codigo_cnes := stringr::str_pad(estabelecimento_codigo_cnes, width = 7, pad = 0)]

# identificar n characters
data_vacinacao[, cnes_nchar := nchar(estabelecimento_codigo_cnes)]
data_vacinacao[, estabelecimento_codigo_cnes := as.character(estabelecimento_codigo_cnes)]

# join
data_vacinacao_log <- left_join(data_vacinacao,
                                data_cnes,
                                by = c("estabelecimento_codigo_cnes" = "CO_CNES")
)

# selecionar colunas
data_vacinacao_log <- data_vacinacao_log %>% 
  select(estabelecimento_codigo_cnes, NO_LOGRADOURO, NU_ENDERECO, NO_BAIRRO, CO_CEP,
         estabelecimento_municipio, estabelecimento_unidade_federativa)


# uniques
data_vacinacao_log <- data_vacinacao_log %>%
  distinct(estabelecimento_codigo_cnes, .keep_all = TRUE)

# criar enderecos
data_vacinacao_log <- data_vacinacao_log %>%
  mutate(endereco = paste0(NO_LOGRADOURO, ", ", NU_ENDERECO, ", ", NO_BAIRRO,
                           " - CEP ", CO_CEP, " - ", estabelecimento_municipio, ", ", estabelecimento_unidade_federativa))

data_vacinacao_log <- arrange(data_vacinacao_log, estabelecimento_codigo_cnes)


ggmap::register_google(fread("../data-raw/painel_vacinacao_covid/google_key.txt")$V1)



coordenadas_google_1_1 <- lapply(X=data_vacinacao_log$endereco[1:2500], ggmap::geocode, output = "all")
coordenadas_google_1_2 <- lapply(X=data_vacinacao_log$endereco[2501:5000], ggmap::geocode, output = "all")
coordenadas_google_2 <- lapply(X=data_vacinacao_log$endereco[5001:10000], ggmap::geocode, output = "all")
coordenadas_google_3 <- lapply(X=data_vacinacao_log$endereco[10001:11000], ggmap::geocode, output = "all")
coordenadas_google_4 <- lapply(X=data_vacinacao_log$endereco[11001:12000], ggmap::geocode, output = "all")
coordenadas_google_5 <- lapply(X=data_vacinacao_log$endereco[12001:13000], ggmap::geocode, output = "all")
coordenadas_google_6 <- lapply(X=data_vacinacao_log$endereco[13001:14000], ggmap::geocode, output = "all")
coordenadas_google_7 <- lapply(X=data_vacinacao_log$endereco[14001:15000], ggmap::geocode, output = "all")
coordenadas_google_8 <- lapply(X=data_vacinacao_log$endereco[15001:16000], ggmap::geocode, output = "all")
coordenadas_google_9 <- lapply(X=data_vacinacao_log$endereco[16001:17000], ggmap::geocode, output = "all")
coordenadas_google_10 <- lapply(X=data_vacinacao_log$endereco[17001:18000], ggmap::geocode, output = "all")
coordenadas_google_11 <- lapply(X=data_vacinacao_log$endereco[18001:19000], ggmap::geocode, output = "all")
coordenadas_google_12 <- lapply(X=data_vacinacao_log$endereco[19001:20000], ggmap::geocode, output = "all")
coordenadas_google_13 <- lapply(X=data_vacinacao_log$endereco[20001:nrow(data_vacinacao_log)], ggmap::geocode, output = "all")

# join them all together
coordenadas_google <- c(coordenadas_google_1_1, coordenadas_google_1_2,
                        coordenadas_google_2, coordenadas_google_3, coordenadas_google_4,
                        coordenadas_google_5,coordenadas_google_6,coordenadas_google_7,coordenadas_google_8,
                        coordenadas_google_9,coordenadas_google_10,coordenadas_google_11,coordenadas_google_12,
                        coordenadas_google_13)



# identify list names as id_estab
names(coordenadas_google) <- data_vacinacao_log$estabelecimento_codigo_cnes

# save
readr::write_rds(coordenadas_google, "../data/painel_vacinacao_covid/locais_vacinacao_geocode_output_google.rds")


# check if there any difference between the estabs that were saved and the estabs that were
# supposed to be geocoded
coordenadas_google <- readr::read_rds("../data/painel_vacinacao_covid/locais_vacinacao_geocode_output_google.rds")

if (length(setdiff(data_vacinacao_log$estabelecimento_codigo_cnes, names(coordenadas_google))) > 0) {
  
  message(sprintf("\nThere are %i new estabs to geocode in gmaps",  
                  length(setdiff(data_vacinacao_log$estabelecimento_codigo_cnes, names(coordenadas_google)))))
  
  # new estab to geocode in gmaps
  estabs_problema_new <- data_vacinacao_log[estabelecimento_codigo_cnes %nin% names(coordenadas_google)]
  
  # send to gmaps
  coordenadas_google1_new <- lapply(X=estabs_problema_new$endereco, ggmap::geocode, output = "all")
  
  # identify list names as id_estab
  names(coordenadas_google1_new) <- estabs_problema_new$estabelecimento_codigo_cnes
  
  # bind to the old geocoded estabs by gmaps1
  coordenadas_google <- c(coordenadas_google, coordenadas_google1_new)
  
  # save it
  readr::write_rds(coordenadas_google, "../data/painel_vacinacao_covid/locais_vacinacao_geocode_output_google.rds")

}

# function to create data.frame from gmaps output
create_dt <- function(x) {
  
  precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
                             x[["results"]][[1]][["address_components"]], 
                             NA)
  
  # check length from precision depth
  precision_depth <- ifelse(is.na(precision_depth0), NA,
                            ifelse(length(precision_depth0[[1]]$types) > 0,
                                   precision_depth0[[1]]$types[[1]], 
                                   NA))
  a <- data.table(
    MatchedAddress = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
    # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
    PrecisionDepth = precision_depth,
    lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
    lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
  )
  
}

# 3.5) Rodar funcao que transforma todos os estabs georef em data.table
estabs_problema_geocoded <- lapply(coordenadas_google, create_dt)

# 3.6) Rbind as data.table
estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "estabelecimento_codigo_cnes",
                                         use.names = TRUE)
# arrange
estabs_problema_geocoded_dt <- arrange(estabs_problema_geocoded_dt, estabelecimento_codigo_cnes)

# identificar endereco procurado
estabs_problema_geocoded_dt[, SearchedAddress := data_vacinacao_log$endereco]

table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')


# segunda rodad: estabs com baixa precisao e que deram NA -----------------------------------------
# esses vao ter ser geocoded com sua razao social
estabs_baixa_precisao <- estabs_problema_geocoded_dt[PrecisionDepth %nin% c("street_number", "route", "airport", 
                                                                            "amusement_park","bus_station","establishment",
                                                                            "intersection",
                                                                            "neighborhood",
                                                                            "political",
                                                                            "post_box",
                                                                            "premise",
                                                                            "subpremise",
                                                                            "postal_code",
                                                                            "town_square") | is.na(PrecisionDepth)]

# selecionar colunas
data_vacinacao_prob <- data_vacinacao %>% 
  filter(estabelecimento_codigo_cnes %in% estabs_baixa_precisao$estabelecimento_codigo_cnes) %>%
  select(estabelecimento_codigo_cnes, estabelecimento, estabelecimento_razao_social,
         estabelecimento_municipio, estabelecimento_unidade_federativa) %>%
  distinct(estabelecimento_codigo_cnes, .keep_all = TRUE)

# criar enderecos
data_vacinacao_prob <- data_vacinacao_prob %>%
  mutate(endereco = paste0(estabelecimento, " - ", estabelecimento_municipio, ", ", estabelecimento_unidade_federativa))
