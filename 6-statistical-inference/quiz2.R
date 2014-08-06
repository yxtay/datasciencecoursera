setwd("~/GitHub/datasciencecoursera/6-statistical-inference/")

# Q2
mu <- 80; sd <- 10
pnorm((70 - mu) / sd)

# Q3
mu <- 1100; sd <- 75
mu + sd * qnorm(0.95)

# Q4
mu <- 1100; sd <- 75; n <- 100
mu + sd / sqrt(n) * qnorm(0.95)

# Q5
1 - pbinom(3, 5, 0.5)

# Q6
mu <- 15; sd <- 10; n <- 100
pnorm((16 - mu) / (sd / sqrt(n))) - pnorm((14 - mu) / (sd / sqrt(n)))

# Q8
mu <- 0.5; sd <- sqrt(1/12); n <- 100
sd / sqrt(n)

# Q9
ppois(10, 3 * 5)

# Q10
x <- c(1,-1); p <- 0.5
sum(x ^ 2 * p) - sum(x * p)
