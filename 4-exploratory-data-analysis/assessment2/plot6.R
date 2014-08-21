# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data to those from Baltimore City or Los Angeles Country and ON-ROAD
# assuming that type == "ON-ROAD" gives all motor vehicle sources
NEI_sub <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD")

# sum emissions by year and location
# requires plyr package
library(plyr)
em_df <- ddply(NEI_sub, .(year, fips), summarise, em = sum(Emissions))

# process year as Date variable and label location appropriately
em_df <- mutate(em_df,
                year = as.Date(paste0(year, "-01-01")),
                Location = factor(fips, levels = c("24510", "06037"), 
                                  labels = c("Baltimore City", "Los Angeles County")))

# plot emissions against year by Location
# requires ggplot2 package
library(ggplot2)
png("plot6.png")
q <- qplot(year, em, data = em_df, col = Location, geom = "line",
           main = "PM2.5 Emissions from Motor Vehicles",
           xlab = "Year", ylab = "Emissions (tons)")
print(q)
dev.off()
