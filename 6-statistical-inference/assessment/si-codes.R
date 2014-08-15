setwd("~/GitHub/datasciencecoursera/6-statistical-inference/assessment/")

set.seed(1)
n <- 40; lambda <- 0.2
samples <- replicate(1000, rexp(n, lambda))
sample.means <- apply(samples, 2, mean)

mean.sample.mean <- mean(sample.means)
mean.sample <- mean(samples)
mu <- 1 / lambda
mean.sample.mean; mean.sample; mu

sd.sample.mean <- sd(sample.means)
sd.sample <- sd(samples) / sqrt(n)
sigma <- 1 / lambda / sqrt(n)
sd.sample.mean; sd.sample; sigma

plot(density(sample.means),
     main = "Density Plot of Mean of exp(40, 0.2)",
     xlab = "Value")
curve(dnorm(x, mu, sigma), add = T, lty = 2)
legend("topright", legend = c("Sample Density", "Normal Density"), lty = 1:2)

qqnorm(sample.means)
qqline(sample.means, col = 2)

CI <- mean.sample + c(-1,1) * sd.sample.mean * 1.96
mean(sample.means > CI[1] & sample.means < CI[2])

data(ToothGrowth)
boxplot(len ~ supp, data = ToothGrowth, main = "Boxplot of Tooth Length against Supplement")
boxplot(len ~ dose, data = ToothGrowth, main = "Boxplot of Tooth Length against Dosage")
boxplot(len ~ dose + supp, data = ToothGrowth, main = "Boxplot of Tooth Length against Supplement and Dosage")

summary(ToothGrowth)
with(ToothGrowth, table(supp, dose))

g1 <- t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))
g2 <- t.test(len ~ supp, data = subset(ToothGrowth, dose == 1))
g3 <- t.test(len ~ supp, data = subset(ToothGrowth, dose == 2))
