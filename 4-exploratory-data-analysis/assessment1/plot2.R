# assume data has been unzipped into directory
# read data
data <- read.table("household_power_consumption.txt", sep = ";", header = T,
                   nrows = 100000, na.string = "?")

# process Data as Date variable and select appropriate dates
data <- within(data, {Date = as.Date(Date, "%d/%m/%Y")})
data <- subset(data, Date %in% as.Date(c("2007-02-01", "2007-02-02")))

# process Data and Time into POSIX variable
data <- within(data, {datetime = strptime(paste(Date,Time), format = "%F %T")})

# plot time series plot of global active power
png("plot2.png", type = "cairo")
with(data, 
     plot(datetime, Global_active_power, type = "l", 
          xlab = "", ylab = "Global Active Power (kilowatts)")
     )
dev.off()