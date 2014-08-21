# assume that data has been unzipped into directory 
# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# find SCC codes corresponding to coal combustion-related sources by scanning for "Coal"
reqSCC <- with(SCC, SCC[grep("Coal", SCC.Level.Three)])

# subset data to only those with SCC from the previous step
NEI_sub <- subset(NEI, SCC %in% reqSCC)

# sum emissions by year
em <- with(NEI_sub, tapply(Emissions, year, sum))

# process year as Data varialbe
year <- as.Date(paste0(names(em), "-01-01"))

# plot emssions against year
png("plot4.png")
plot(year, em / 10^3, type = "l",
     main = "PM2.5 Emissions from Coal Combustion",
     xlab = "Year", ylab = "Emissions (thousand tons)")
dev.off()
