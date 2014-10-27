# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data to those from Baltimore City and ON-ROAD
# assuming that type == "ON-ROAD" gives all motor vehicle sources
NEI_sub <- subset(NEI, fips == "24510" & type == "ON-ROAD")

# sum emissions by year
em_Bt_road <- with(NEI_sub, tapply(Emissions, year, sum))

# process year as Date variable
year <- as.Date(paste0(rownames(em_Bt_road), "-01-01"))

# plot emissions against year
png("plot5.png")
plot(year, em_Bt_road, type = "l",
      main = "PM2.5 Emissions from Motor Vehicles in Baltimore City",
      xlab = "Year", ylab = "Emissions (tons)")
dev.off()