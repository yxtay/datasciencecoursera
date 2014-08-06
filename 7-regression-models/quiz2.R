# Q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)$coef[2,4]

# Q2
summary(fit)$sigma

# Q3
fit <- lm(mpg ~ wt, data = mtcars)
predict(fit, data.frame(wt = mean(mtcars$wt)), interval = "confidence")

# Q4
?mtcars

# Q5
predict(fit, data.frame(wt = 3), interval = "prediction")

# Q6
confint(fit)[2,] * 2

# Q9
1 - summary(fit)$r.squared
