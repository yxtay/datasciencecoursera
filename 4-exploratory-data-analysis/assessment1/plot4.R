if(file.exists("household_power_consumption_sub.csv")) {
    # read processed data to save computation times
    data <- read.csv("household_power_consumption_sub.csv", header = T, stringsAsFactor = F)
    data <- within(data, {Date <- as.Date(Date)
                          datetime <- strptime(datetime, "%F %T")})
} else {
    # read and process file from scratch
    if(!file.exists("household_power_consumption.txt")) unzip("exdata-data-household_power_consumption.zip")
    data <- read.table("household_power_consumption.txt", sep = ";", header = T, 
                       na.string = "?", stringsAsFactor = F)
    
    # process Data as Date variable and select appropriate dates
    data <- within(data, {Date = as.Date(Date, "%d/%m/%Y")})
    data <- subset(data, Date %in% as.Date(c("2007-02-01", "2007-02-02")))
    
    # process Data and Time into POSIX variable
    data <- within(data, {datetime = strptime(paste(Date,Time), format = "%F %T")})
    
    # output processed subset file to reduce read times
    write.csv(data, "household_power_consumption_sub.csv", row.names = F)
}

# plot 4 panels
png("plot4.png", type = "cairo")
op <- par(mfrow = c(2, 2))
with(data, {
    # plot time series of global active power
    plot(datetime, Global_active_power, type = "l", 
         xlab = "", ylab = "Global Active Power")
    
    # plot time series of voltage
    plot(datetime, Voltage, type = "l",)
    
    # plot time series of energy sub metering with legend
    metering <- paste0("Sub_metering_", 1:3)
    colors <- c("black", "red", "blue")
    matplot(as.POSIXct(datetime), data[, metering],
            type = "l", lty = "solid", col = colors,
            xlab = "" , xaxt = "n", ylab = "Energy sub metering")
    axis.POSIXct(1, datetime)
    legend("topright", legend = metering, col = colors, lty = "solid", bty = "n")
    
    # plot time series of global reactive power
    plot(datetime, Global_reactive_power, type = "l")
})
par(op)
dev.off()