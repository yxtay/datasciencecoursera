# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# subset data to only those in Baltimore City
NEI_sub <- subset(NEI, fips == "24510")

# sum emissions by year and type
# requires plyr package
library(plyr)
em_df <- ddply(NEI_sub, .(year, type), summarise, em = sum(Emissions))

# process year as Date variable
em_df <- mutate(em_df,
                year = as.Date(paste0(year, "-01-01")),
                type = gsub("-", "", tolower(type)))

# plot emissions against year by type
# requires ggplot2 package
library(ggplot2)
png("plot3.png")
q <- qplot(year, em, data = em_df, col = type, geom = "line",
           main = "PM2.5 Emissions in Baltimore City by Source",
           xlab = "Year", ylab = "Emissions (tons)") +
    scale_color_discrete("Type", breaks = c("point", "nonpoint", "onroad", "nonroad"))
print(q)
dev.off()