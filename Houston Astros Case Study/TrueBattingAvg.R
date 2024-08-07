library(extraDistr)  # for the beta-binomial distribution functions

alpha <- 101
beta <- 199
n <- 500
threshold <- 200

probability <- 1 - pbbinom(threshold - 1, n, alpha, beta)
print(probability)