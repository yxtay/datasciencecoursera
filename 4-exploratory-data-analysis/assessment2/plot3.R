# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data to only those in Baltimore City
NEI_sub <- subset(NEI, fips == "24510")

# sum emissions by year and type
em_Bt_type <- with(NEI_sub, tapply(Emissions, list(year, type), sum))
em_Bt_type <- as.data.frame(em_Bt_type)

# process year as Data variable and bind to data frame
year <- as.Date(paste(rownames(em_Bt_type), "-01-01", sep = ""))
em_Bt_type <- cbind(year, em_Bt_type)
names(em_Bt_type) <- tolower(names(em_Bt_type))
names(em_Bt_type) <- gsub("-", "", names(em_Bt_type))

# convert data frame to long format for use by ggplot
# requires reshape2 package for melt() function
library(reshape2)
em_melt <- melt(em_Bt_type, measure.vars = c("point", "nonpoint", "onroad", "nonroad"))
names(em_melt) <- c("Year", "Type", "Emissions")

# plot emissions against year by type
# requires ggplot2 package
library(ggplot2)
png("plot3.png")
q <- qplot(Year, Emissions, data = em_melt, col = Type, geom = "line",
           main = "PM2.5 Emissions in Baltimore City by Source",
           xlab = "Year", ylab = "Emissions (tons)")
print(q)
dev.off()
