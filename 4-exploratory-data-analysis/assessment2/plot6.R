# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data to those from Baltimore City or Los Angeles Country and ON-ROAD
# assuming that type == "ON-ROAD" gives all motor vehicle sources
NEI_sub <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD")

# sum emissions by year
em <- with(NEI_sub, tapply(Emissions, list(year, fips), sum))

# convert output to data frame and name variables appropriately
em <- as.data.frame(em)
names(em) <- c("Los Angeles County", "Baltimore City")

# process year as Data variable and bind to data frame
year <- as.Date(paste(rownames(em), "-01-01", sep = ""))
em <- cbind(year, em)

# convert data frame to long format for us by ggplot
# requires reshape2 package for melt() function
library(reshape2)
em_melt <- melt(em, measure.vars = c("Los Angeles County", "Baltimore City"))
names(em_melt) <- c("Year", "Location", "Emissions")

# plot emissions against year by Location
library(ggplot2)
png("plot6.png")
q <- qplot(Year, Emissions, data = em_melt, col = Location, geom = "line",
           main = "PM2.5 Emissions from Motor Vehicles",
           xlab = "Year", ylab = "Emissions (tons)")
print(q)
dev.off()