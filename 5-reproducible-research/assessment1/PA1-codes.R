setwd("~/GitHub/datasciencecoursera/5-reproducible-research/assessment1/")

## Loading and preprocessing the data

if(!file.exists("activity.csv")) unzip("activity.zip")
df <- read.csv("activity.csv")

df <- within(df, {date <-  as.Date(date)
                  interval <- sprintf("%04d", interval)
                  datetime <- strptime(paste(date, interval), "%F %H%M")})

## What is mean total number of steps taken per day?

day_steps <- with(df, tapply(steps, date, sum, na.rm = T))

with(df, hist(day_steps,
              main = "Histogram of Total Step Count",
              xlab = "Total step count"))
steps_mean <- mean(day_steps)
steps_median <- median(day_steps)

## What is the average daily activity pattern?

time <- strptime(unique(df$interval), "%H%M")
avg_steps <- with(df, tapply(steps, interval, mean, na.rm = T))

plot(time, avg_steps, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "Time", ylab = "Average step count")
steps_max <- max(avg_steps)
max_time <- format(time[which.max(avg_steps) + 0:1], "%H:%M")

## Imputing missing values

id_na <- is.na(df$steps)
NAcount <- sum(id_na)

time_chr <- format(time, "%H%M")
idx <- with(df, match(interval[id_na], time_chr))
df2 <- within(df, {steps[id_na] <- avg_steps[idx]})

day_steps2 <- with(df2, tapply(steps, date, sum, na.rm = T))
with(df, hist(day_steps2,
              main = "Histogram of Total Step Count",
              xlab = "Total step count"))
steps_mean2 <- mean(day_steps2)
steps_median2 <- median(day_steps2)

## Are there differences in activity patterns between weekdays and weekends?

df2 <- within(df2, {day <- weekdays(date)
                    weekend <- ifelse(is.element(day, c("Saturday", "Sunday")), "weekend", "weekday")})

steps <- with(df2, tapply(steps, list(interval, weekend), mean))
df3 <- cbind(time, as.data.frame(steps))

par(mfrow = c(2,1), mar = c(4,4,2,1))
plot(weekday ~ time, data = df3, type = "l",
     main = "Weekday",
     xlab = "Time", ylab = "Average step count")
plot(weekend ~ time, data = df3, type = "l",
     main = "Weekend",
     xlab = "Time", ylab = "Average step count",
     ylim = range(df3$weekday))
par(mfrow = c(1,1))

## knit PA1_template.Rmd
library(knitr)
knit2html("PA1_template.Rmd")
