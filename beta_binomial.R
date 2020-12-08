

set.seed(2) # semente
rbeta(3, 3,4)

hist(rbeta(10000, .5, .5))
hist(rbeta(10000, 1,1))
hist(rbeta(10000, 3, 4))

# lançamento da moeda
n <- 50
s <- 30

# verossimilhança é binomial (n=50, s=30)

# Priori uniforme
# Beta(1,1)
a1 <- 1
a2 <- 1

# posteriori = B(a1 + s, a2 + n -s)

# historgrama da posteriori
hist(rbeta(10000, a1 + s, a2 + n - s))

# qual a probabilidade de theta > 50%?

sim_posteiori <- rbeta(10000, a1 + s -1, a2 + n - s -1)
head(sim_posteiori)
sum(sim_posteiori > .5)/10000
mean(sim_posteiori)
