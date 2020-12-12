library(brms)
schools_dat <- list(J = seq(1,8,1), 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

set.seed(2)
model1 <- brm(y|se(sigma) ~ 1 + (1|J), data = schools_dat,
              iter = 3000)

## alternativa
schools <- data.frame(y = c(28,  8, -3,  7, -1,  1, 18, 12),
                      sigma = c(15, 10, 16, 11,  9, 11, 10, 18),
                      J = 1:8)

summary(model1)
coef(model1)$J 
#to extract the summary estimates of theta and coef(<brmsmodel>, summary = FALSE)$J t


model2 <- brm(y|se(sigma) ~ 1 + (1|J), data = schools)