# assume data has been unzipped into directory
# read data
data <- read.table("household_power_consumption.txt", sep = ";", header = T,
                   nrows = 100000, na.string = "?")

# process Data as Date variable and select appropriate dates
data <- within(data, {Date = as.Date(Date, "%d/%m/%Y")})
data <- subset(data, Date %in% as.Date(c("2007-02-01", "2007-02-02")))

# process Data and Time into POSIX variable
data <- within(data, {datetime = strptime(paste(Date,Time), format = "%F %T")})

# plot time series of energy sub metering with legend
png("plot3.png", type = "cairo")
with(data, {
    plot(datetime, Sub_metering_1, type = "l", 
         xlab = "", ylab = "Energy sub metering")
    lines(datetime, Sub_metering_2, col = "red")
    lines(datetime, Sub_metering_3, col = "blue")
    legend("topright", 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col = c("black", "red", "blue"), lty = 1)
})
dev.off()