setwd("~/GitHub/datasciencecoursera/2-r-programming/")

# Q1
cube <- function(x, n) {
  x^3
}
cube(3)

# Q3
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3)

# Q4
x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}