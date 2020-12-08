# carrega bibliotecas
library(data.table)
library(tidyverse)
library(here)

setwd(here())

# importa banco de dados de mortes por armas de fogo nos EUA, 2012-2014 do 538
# fonte: https://www.kaggle.com/jameslko/gun-violence-data
gun <- fread("archive/gun-violence-data_01-2013_03-2018.csv")

# limpa os dados e coloca em formato que queremos, de dados por mês ano
gun <- gun %>%
  mutate(month = as.Date(date),
         month = format(month, "%Y-%m"),
         gender_female = str_detect(participant_gender , "Female")) %>%
  group_by(month, state) %>%
  summarise(num_casos = n(),
            num_mortes = sum(n_killed),
            all_male_participants = sum(!gender_female))

# importa dados de permissões para compras de armas de fogo

checks <- fread("https://raw.githubusercontent.com/raksha592/Data-Analysis-on-Gun-Violence/master/nics-firearm-background-checks.csv")


# seleciona variáveis que preciso
checks <- checks %>%
  dplyr::select(month, state, permit, totals)

# Junta os bancos de dados e cria variável que é razão entre permissão de compra e total vendido
guns_checks <- gun %>%
  inner_join(checks, by = c("month" = "month", "state" = "state")) %>%
  mutate(ratio_permit_totals = permit/totals) %>%
  ungroup()

saveRDS(guns_checks, file="Clean_data/guns_data.RDS")
