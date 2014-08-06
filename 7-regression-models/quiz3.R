# Q1
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)$coef

# Q2
fit2 <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit2)$coef

# Q3
fit3 <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
anova(fit, fit3)

# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
max(hat(x))

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
dfbetas(fit)[which.max(hatvalues(fit)),]
