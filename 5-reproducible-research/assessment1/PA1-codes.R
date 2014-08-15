setwd("~/GitHub/datasciencecoursera/5-reproducible-research/assessment1/")

library(plyr)
library(ggplot2); library(scales)

## Loading and preprocessing the data

if(!file.exists("activity.csv")) unzip("activity.zip")
df <- read.csv("activity.csv")

df <- mutate(df,
             date = as.Date(date),
             interval = sprintf("%04d", interval),
             time = strptime(interval, "%H%M"),
             datetime = strptime(paste(date, interval), "%F %H%M"))

## What is mean total number of steps taken per day?

df_days <- ddply(df, .(date), summarise, steps = sum(steps, na.rm = T))

hist(df_days$steps,
     main = "Histogram of Number of Steps",
     xlab = "Number of steps")
qplot(steps, data = df_days, geom = "histogram", binwidth = 2500,
      main = "Histogram of Number of Steps",
      xlab = "Number of steps", ylab = "Count")
steps_mean <- mean(df_days$steps)
steps_median <- median(df_days$steps)

## What is the average daily activity pattern?

df_daily <- ddply(df, .(time), summarise, steps = mean(steps, na.rm = T))

with(df_daily, plot(time, steps, type = "l",
                    main = "Average Daily Activity Pattern",
                    xlab = "Time", ylab = "Number of steps"))
qplot(time, steps, data = df_daily, geom = "line",
      main = "Average Daily Activity Pattern",
      xlab = "Time", ylab = "Number of steps") +
    scale_x_datetime(labels = date_format("%H:%M"))
steps_max <- max(df_daily$steps)
max_interval <- with(df_daily, format(time[which.max(steps) + 0:1], "%H:%M"))

## Imputing missing values

id_na <- is.na(df$steps)
NAcount <- sum(id_na)

idx <- match(df$time[id_na], df_daily$time)
df2 <- within(df, {steps[id_na] <- df_daily$steps[idx]})

df_daily2 <- ddply(df2, .(date), summarise, steps = sum(steps, na.rm = T))
hist(df_daily2$steps,
     main = "Histogram of Total Step Count",
     xlab = "Total step count")
qplot(steps, data = df_daily2, geom = "histogram", binwidth = 2500,
      main = "Histogram of Total Step Count",
      xlab = "Total step count", ylab = "Count")
steps_mean2 <- mean(df_daily2$steps)
steps_median2 <- median(df_daily2$steps)

## Are there differences in activity patterns between weekdays and weekends?

df2 <- mutate(df2,
              day = weekdays(date),
              weekend = ifelse(is.element(day, c("Saturday", "Sunday")), "Weekend", "Weekday"))

df_weekend <- ddply(df2, .(time, weekend), summarise,
                    steps = mean(steps))

qplot(time, steps, data = df_weekend, geom = "line", facets = weekend ~ .,
      xlab = "Time", ylab = "Number of steps") +
    scale_x_datetime(labels = date_format("%H:%M"))

## knit PA1_template.Rmd
library(knitr)
knit2html("PA1_template.Rmd")
