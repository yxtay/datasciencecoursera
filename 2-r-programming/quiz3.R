setwd("~/GitHub/datasciencecoursera/2-r-programming/")

# Q1
library(datasets)
data(iris)
with(iris, mean(Sepal.Length[Species == "virginica"]))

# Q2
apply(iris[, 1:4], 2, mean)

# Q3
library(datasets)
data(mtcars)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

# Q4
x <- with(mtcars, tapply(hp, cyl, mean))
abs(x[1] - x[3])