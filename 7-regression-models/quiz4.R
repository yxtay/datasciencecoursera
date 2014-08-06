# Q1
library(MASS)
fit <- glm(use ~ wind, data = shuttle, family = binomial)
exp(fit$coef[2])

# Q2
fit <- glm(use ~ wind + magn, data = shuttle, family = binomial)
exp(fit$coef[2])

# Q3
fit <- glm(count ~ spray, data = InsectSprays, family = poisson)
exp(-fit$coef[2])

# Q5
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
xKnot <- (x > 0) * x
xMat <- cbind(1, x, xKnot)
fit <- lm(y ~ xMat - 1)
sum(fit$coef[2:3])
