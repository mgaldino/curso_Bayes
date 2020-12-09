library(brms)
library(here)
library(tidyverse)

setwd(here())

guns <- readRDS("Clean_data/guns_data.RDS")

# inspecionando os dados
glimpse(guns)

reg_freq <- lm(num_mortes ~ ratio_permit_totals + all_male_participants, data=guns)
summary(reg_freq)

reg_bayes <- brms::brm(num_mortes ~ ratio_permit_totals + all_male_participants, data=guns)

# generate a summary of the results
summary(reg_freq) # frequentista
summary(reg_bayes) #Bayes

# plot the MCMC chains as well as the posterior distributions
plot(reg_bayes, ask = FALSE)

# yi ~ N(mui, sigma)
# mui = a + b*ratio_permit_totalsi + c*all_male_participantsi

# yi = a + b*ratio_permit_totalsi + c*all_male_participantsi + e_i
# e_i ~N(0, sigma)

# Y = a + bX


# investigate model fit
pp_check(reg_bayes)

## Prioris
(prior <- get_prior(num_mortes ~ ratio_permit_totals + all_male_participants, data=guns))

# parametrização da T: graus de liberdade, média e escala
# vamos entender?
library(LaplacesDemon)

# prior para sigma (dp)
my_t_sigma <- LaplacesDemon::rst(1000, nu=3, mu=0, sigma=16.3)
hist(my_t_sigma)

# prior para intercepto
my_t_intercept <- LaplacesDemon::rst(1000, nu=3, mu=13, sigma=16.3)
hist(my_t_intercept)

summary(reg_bayes) #Bayes

# vamos especificar nossas prioris?
prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "ratio_permit_totals"),
            set_prior("normal(0,1)", class = "b", coef = "all_male_participants"))
# deixei sigma pro programa.

reg_bayes1 <- brms::brm(num_mortes ~ ratio_permit_totals + all_male_participants, data=guns,
                        prior = prior1)

summary(reg_bayes1)
summary(reg_bayes)
# 

# se pensarmos bem, espero que o efeito de homens participando em mortes por armas de fogo é aumentar número de mortes
# e a variável ratio, deve ter efeito negativo
# na verdade, deveria pensar nisso antes de rodar a regressão
# afinal, não posso "roubar" com uma priori que confirme os dados após ver os dados
# mas aqui, por razões pedagógicas, apresento agora.

prior1 <- c(set_prior("normal(-1,10)", class = "b", coef = "ratio_permit_totals"),
            set_prior("normal(1,10)", class = "b", coef = "all_male_participants"),
            set_prior("normal(0,10)", class = "Intercept"))


