library(brms)


# y_j ~ N(teta_j, se_j)
# teta_j ~ N(mu, tau)

# y_j = a_j + e_j, e_j ~ N(0, se_j)
# a_j = mu 

schools_dat <- list(J = 1:8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

set.seed(2)
model1 <- brm(y|se(sigma) ~ 1 + (1|J), data = schools_dat,
              iter = 3000)

summary(model1)
coef(model1)$J 

prior_summary(model1)
get_prior(y|se(sigma) ~ 1 + (1|J), data = schools_dat)

#to extract the summary estimates of theta and coef(<brmsmodel>, summary = FALSE)$J t

set.seed(2)
prior_tau <- set_prior("normal(0,.2)", class = "sd", coef = "Intercept", group = "J")

model2 <- brm(y|se(sigma) ~ 1 + (1|J), data = schools_dat,
              iter = 5000, prior = prior_tau)
summary(model2)
coef(model2)$J 



