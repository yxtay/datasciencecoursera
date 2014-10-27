# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data for only those in Baltimore City
NEI_sub <- subset(NEI, fips == "24510")

# sum emissions from all sources by year
em_Bt <- with(NEI_sub, tapply(Emissions, year, sum))

# process year as Date variable
year <- as.Date(paste0(names(em_Bt), "-01-01"))

# plot emissions against year
png("plot2.png")
plot(year, em_Bt, type = "l",
     main = "PM2.5 Emssions in Baltimore City, Maryland",
     xlab = "Year", ylab = "Emissions (tons)")
dev.off()