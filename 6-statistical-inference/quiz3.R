setwd("~/GitHub/datasciencecoursera/6-statistical-inference/")

# Q1
mu <- 1100; sd <- 30; n <- 9
mu + c(-1,1) * sd / sqrt(n) * qt(0.975, n-1)

# Q2
diff <- -2; n <- 9
abs(diff) / qt(0.975, n-1) * sqrt(n)

# Q4
x <- 3; sdx <- sqrt(0.6); nx <- 10
y <- 5; sdy <- sqrt(0.68); ny <- 10
diff <- x - y
sd <- sqrt(((nx-1) * sdx^2 + (ny-1) * sdy^2) / (nx + ny - 2))
diff + c(-1,1) * sd * sqrt(1/nx + 1/ny) * qt(0.975, nx+ny-2)

# Q6
x <- 6; sdx <- sqrt(2); nx <- 100
y <- 4; sdy <- sqrt(0.5); ny <- 100
diff <- x - y
sd <- sqrt(((nx-1) * sdx^2 + (ny-1) * sdy^2) / (nx + ny - 2))
diff + c(-1,1) * sd * sqrt(1/nx + 1/ny) * qt(0.975, nx+ny-2)

# Q7
x <- -3; sdx <- 1.5; nx <- 9
y <- 1; sdy <- 1.8; ny <- 9
diff <- x - y
sd <- sqrt(((nx-1) * sdx^2 + (ny-1) * sdy^2) / (nx + ny - 2))
diff + c(-1,1) * sd * sqrt(1/nx + 1/ny) * qt(0.95, nx + ny - 2)
