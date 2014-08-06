# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
df <- read.csv(unzip("activity.zip"))
```

It would also be appropriate to preprocess the date and time variables.


```r
df <- within(df, {date <-  as.Date(date)
                  interval <- sprintf("%04d", interval)
                  datetime <- strptime(paste(date, interval), "%F %H%M")})
```


## What is mean total number of steps taken per day?

It would first be necessary to sum the number of steps by day. 
This can be achieved by using the `tapply()` function.


```r
day_steps <- with(df, tapply(steps, date, sum, na.rm=T))

with(df, hist(day_steps,
              main = "Histogram of Total Step Count",
              xlab = "Total step count"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
steps_mean <- mean(day_steps)
steps_median <- median(day_steps)
```

The mean number of steps per day is **9354**.

The median number of steps per day is **10395**.

## What is the average daily activity pattern?

The average number of steps at each time period can be computed with the use of the `tapply()` function.


```r
time <- strptime(unique(df$interval), "%H%M")
avg_steps <- with(df, tapply(steps, interval, mean, na.rm = T))

plot(time, avg_steps, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "Time", ylab = "Average step count")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
steps_max <- max(avg_steps)
max_time <- format(time[which.max(avg_steps) + 0:1], "%H:%M")
```

The maximum average number of steps of **206.17** happened between **08:35** and **08:40**.

## Imputing missing values


```r
id_na <- is.na(df$steps)
NAcount <- sum(id_na)
```

The total number of missing values is **2304**.

I chose to fill the missing values by taking the average number of steps at
each time period which was calculated in the previous part.
It would be necessary to match the missing value to the appropriate average value by time period,
which is achieved by the following codes:


```r
time_chr <- format(time, "%H%M")
idx <- with(df, match(interval[id_na], time_chr))
df2 <- within(df, {steps[id_na] <- avg_steps[idx]})
```

The histogram is then plotted and the mean and median computed.


```r
day_steps2 <- with(df2, tapply(steps, date, sum, na.rm = T))
with(df, hist(day_steps2,
              main = "Histogram of Total Step Count",
              xlab = "Total step count"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
steps_mean2 <- mean(day_steps2)
steps_median2 <- median(day_steps2)
```

The mean and median number of steps per day are **10766** and **10766** respectively.

These values differ from the estimatess from earlier.
From the histogram, there was an increase in the number of days where
the total number of steps fall within 10000 and 15000.
The mean and median had also increased.
On closer inspection of the original data frame, when missing values happened,
they happened throughout the whole day.
The data from these days were replaced by the average daily pattern,
hence all of them had the same total number of steps.
There were 8 such days. 

## Are there differences in activity patterns between weekdays and weekends?

The data frame was first split into data for weekdays and another one for weekends.
Next, the number of steps per 5 min interval was averaged over all the days by interval
in each data frame before a time series plot was plotted.


```r
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
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
par(mfrow = c(1,1))
```

From the panel plots, it could be observed that the average step count for weekdays
was generally higher in the morning before 10:00. 
This indicated that the user was more active during early morning in weekdays than in weekends.
After that, there was not much difference for the rest of the day. 
Although the step count may be slightly higher in the weekend for the rest of the day,
the difference was not huge.
