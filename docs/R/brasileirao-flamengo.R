
# Fonte dos Dados ---------------------------------------------------------

# https://www.kaggle.com/adaoduque/campeonato-brasileiro-de-futebol


# Pacotes -----------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(lubridate)
# library(httr)
# library(rvest)

# Lendo os dados ----------------------------------------------------------

brasileirao <- read_csv2(
  file = "./dados/campeonato-brasileiro-full.csv",
  col_types = cols(
    Data = col_date(format = "%d/%m/%Y")
  )
) %>%
  clean_names()

# link_campeoes <- "https://pt.wikipedia.org/wiki/Lista_de_campe%C3%B5es_do_futebol_do_Brasil"
#
# campeoes <- GET(link_campeoes) %>%
#   content() %>%
#   html_table() %>%
#   .[[1]] %>%
#   clean_names() %>%
#   select(1:2) %>%
#   filter(ano >= 2000 & ano < 2021) %>%
#   rename(campeao = campeonato_brasileiro)

# Transformando os dados --------------------------------------------------

# Brasileirao

brasileirao_limpo <- brasileirao %>%
  filter(data <= as.Date("2021-02-25")) %>%
  mutate(
    ano = year(data) %>% ifelse(. == 2021, 2020, .),
    vencedor = ifelse(vencedor == "-", NA, vencedor)
  ) %>%
  mutate(
    across(
      .cols = c(mandante, visitante, vencedor),
      .fns = str_squish
    )
  ) %>%
  mutate(
    across(
      .cols = c(mandante, visitante, vencedor),
      .fns = str_to_title
    )
  ) %>%
  filter(ano >= 2003) %>%
  select(-starts_with("estado"), -dia, -horario, -arena, -data) %>%
  relocate(ano, .after = rodada)

# Flamengo

flamengo <- brasileirao_limpo %>%
  filter(mandante == "Flamengo" | visitante == "Flamengo") %>%
  mutate(
    gols_marcados = case_when(
      mandante == "Flamengo" ~ mandante_placar,
      visitante == "Flamengo" ~ visitante_placar
    ),
    gols_sofridos = case_when(
      mandante == "Flamengo" ~ visitante_placar,
      visitante == "Flamengo" ~ mandante_placar
    ),
    mando_campo = ifelse(
      mandante == "Flamengo", "Mandante", "Visitante"
    ) %>%
      as.factor(),
    resultado = case_when(
      mandante == "Flamengo" & mandante_placar > visitante_placar ~ "Vitória",
      mandante == "Flamengo" & mandante_placar < visitante_placar ~ "Derrota",
      mandante == "Flamengo" & mandante_placar == visitante_placar ~ "Empate",
      visitante == "Flamengo" & mandante_placar < visitante_placar ~ "Vitória",
      visitante == "Flamengo" & mandante_placar > visitante_placar ~ "Derrota",
      visitante == "Flamengo" & mandante_placar == visitante_placar ~ "Empate"
    ) %>%
      as.factor()
  ) %>%
  select(!c(rodada, mandante:visitante_placar))



