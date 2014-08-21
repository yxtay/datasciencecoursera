# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# sum emissions from all sourse by year
em <- with(NEI, tapply(Emissions, year, sum))

# process year as Data variable
year <- as.Date(paste0(names(em), "-01-01"))

# plot emissions against year
png("plot1.png")
plot(year, em / 10^6, type = "l",
     main = "PM2.5 Emssions in USA",
     xlab = "Year", ylab = "Emissions (million tons)")
dev.off()
