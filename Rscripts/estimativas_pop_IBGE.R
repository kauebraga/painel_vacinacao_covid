library(tidyverse)

# open data -----------------------------------------------------------------------------------
estimativas_ibge <- readxl::read_xls("../../data-raw/painel_vacinacao_covid/estimativa_dou_2020.xls",
                                      sheet = 2, skip = 1)


# fix columns ---------------------------------------------------------------------------------

estimativas_ibge <- estimativas_ibge %>%
  # format column names
  janitor::clean_names() %>%
  mutate(code_muni = paste0(cod_uf, cod_munic)) %>%
  select(uf, code_muni, name_muni = nome_do_municipio, populacao_estimada)

# some population numbers have a (1) notation, delete it
estimativas_ibge <- estimativas_ibge %>%
  mutate(populacao_estimada = stringr::str_replace(populacao_estimada,
                                                  "\\(\\d{1,}\\)",
                                                  "")) %>%
  mutate(populacao_estimada = as.integer(populacao_estimada)) %>%
  # delete na
  filter(!is.na(populacao_estimada))


# svae ot
write_rds(estimativas_ibge, "../../data/painel_vacinacao_covid/estimativas_pop_ibge_2020.rds")



