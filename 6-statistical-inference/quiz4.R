setwd("~/GitHub/datasciencecoursera/6-statistical-inference/")

# Q1
x <- c(140,138,150,148,135)
y <- c(132,135,151,146,130)
t.test(x, y, paired = T)

# Q2
mu <- 1100; sd <- 30; n <- 9
mu + c(-1,1) * sd / sqrt(n) * qt(0.975, n-1)

# Q3
binom.test(3, 4, alternative = "greater")

# Q4
ppois(10, 1787 / 100)

# Q5
x <- -3; sdx <- 1.5; nx <- 9
y <- 1; sdy <- 1.8; ny <- 9
diff <- x - y
sd <- sqrt(((nx-1) * sdx^2 + (ny-1) * sdy^2) / (nx + ny - 2))
1 - pt(abs(diff) / (sd * sqrt(1/nx + 1/ny)), nx + ny - 2)

# Q7
n <- 100; delta <- 0.01; sd <- 0.04; alpha <- 0.05
power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha, type = "one.sample", alternative = "one.sided")

# Q8
delta <- 0.01; sd <- 0.04; alpha <- 0.05; power <- 0.9
power.t.test(delta = delta, sd = sd, sig.level = alpha, power = power, type = "one.sample", alternative = "one.sided")

# Q10
x <- 44; sdx <- 12; nx <- 288
y <- 42.04; sdy <- 12; ny <- 288
diff <- x - y
sd <- sqrt(((nx-1) * sdx^2 + (ny-1) * sdy^2) / (nx + ny - 2))
2 * pt(abs(diff) / (sd * sqrt(1/nx + 1/ny)), nx + ny - 2, lower.tail = F)
