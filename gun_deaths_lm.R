# carrega bibliotecas
library(data.table)
library(tidyverse)
library(here)
library(rstanarm)

setwd(here())
guns_checks <- readRDS("Clean_data/guns_data.RDS")

# inspeciona os dados importados
glimpse(guns_checks)

# regressão linear padrão
# número de mortes em função de se homens envolvidos, e razão entre total de vendas permitidas e total vendido
reg <- lm(num_mortes ~ all_male_participants +  ratio_permit_totals,
          data = guns_checks)
summary(reg)

# Bayes
# modelo y ~ N(mu, sigma_2)
#  mu = a0 + a1*x1 + a2*x2
# ou, y ~ N(a0 + a1*x1 + a2*x2, sigma_2)
# 4 parâmetros a estimar: a0, a1, a2 e sigma_2
# precisamos de priori para cada parâmetro, incluindo a variância.

bayes_reg <- stan_glm(num_mortes ~ all_male_participants +  ratio_permit_totals,
                      data = guns_checks)

# prioris escolhidas por defautl
# quais prioris? Vamos checar?
prior_summary(bayes_reg)


