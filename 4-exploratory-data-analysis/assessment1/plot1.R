# assume data has been unzipped into directory
# read data
data <- read.table("household_power_consumption.txt", sep = ";", header = T,
                   nrows = 100000, na.string = "?")

# process Data as Date variable and select appropriate dates
data <- within(data, {Date = as.Date(Date, "%d/%m/%Y")})
data <- subset(data, Date %in% as.Date(c("2007-02-01", "2007-02-02")))

# plot histogram of global active power
png("plot1.png", type = "cairo")
with(data, 
     hist(Global_active_power, col = "red",
          main = "Global Active Power", xlab = "Global Active Power (kilowatts)"))
dev.off()