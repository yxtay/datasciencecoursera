setwd("~/Github/datasciencecoursera/5-reproducible-research/assessment2/")

library(stringr)
library(plyr)

filename <- "repdata-data-StormData.csv.bz2"
system.time(storm_df <- read.csv(filename, stringsAsFactors = F))

storm_df <- within(storm_df, {BGN_DATE <- as.Date(BGN_DATE, "%m/%e/%Y 0:00:00")})
storm_df2 <- subset(storm_df, BGN_DATE > "1995-12-31")

names(storm_df2) <- tolower(names(storm_df2))
var_keep <- names(storm_df2)[c(2:4,7:8,23:28)]
var_keep
storm_df2 <- subset(storm_df2, select = var_keep)

table(storm_df2$propdmgexp)
table(storm_df2$cropdmgexp)
expchr <- c("K", "M", "B"); exp10 <- 10 ^ c(3, 6, 9, 0)
storm_df2 <- mutate(storm_df2, 
                    propdmgmult = exp10[match(propdmgexp, expchr, nomatch = 4)],
                    cropdmgmult = exp10[match(cropdmgexp, expchr, nomatch = 4)],
                    propdmg = propdmg * propdmgmult,
                    cropdmg = cropdmg * cropdmgmult,
                    totaldmg = propdmg + cropdmg,
                    casualties = fatalities + injuries,
                    evtype = tolower(evtype))

storm_df3 <- subset(storm_df2, totaldmg != 0 | casualties != 0, 
                    select = c(evtype, fatalities, injuries, casualties, propdmg, cropdmg, totaldmg))

storm_sum <- ddply(storm_df3, .(evtype), colwise(sum))

print(n <- sapply(storm_sum[, c(4,7)], 
                  function(x, prop) which((cumsum(sort(x, decreasing = T)) / sum(x)) > prop)[1], 
                  prop = 0.99)
id_list <- mapply(function(x, n) head(order(x, decreasing = T), n), storm_sum[, c(4,7)], n)
id <- sort(unique(do.call(c, id_list)))
length(id)

events <- storm_sum$evtype[id]
event_type <- c("astronomical low tide", "avalanche", "blizzard", "coastal flood", 
                "cold/wind chill", "dense fog", "dense smoke", "drought",  
                "dust devil", "dust storm", "excessive heat", "extreme cold/wind chill",
                "flash flood", "flood", "freezing fog", "frost/freeze",
                "funnel cloud", "hail", "heat", "heavy rain",
                "heavy snow", "high surf", "high wind", "hurricane/typhoon", 
                "ice storm", "lakeshore flood", "lake-effect snow", "landslide",
                "lightning", "marine hail", "marine high wind", "marine strong wind", 
                "marine thunderstorm wind", "rip current", "seiche", "sleet",
                "storm surge/tide", "strong wind", "thunderstorm wind", "tornado",
                "tropical depression", "tropical storm", "tsunami", "volcanic ashfall",
                "watersprout", "wildfire", "winter storm", "winter weather")
events2 <- event_type[match(events, event_type)]
events[is.na(events2)]
events2[is.na(events2)] <- c("extreme cold/wind chill", "dense fog", "winter weather", "excessive heat", "high surf",
                             "high surf", "hurricane/typhoon", "rip current", "storm surge/tide", "thunderstorm wind",
                             "thunderstorm wind", "flood", "wildfire", "strong wind", "winter weather",
                             "winter weather", "winter weather")
all(events2 %in% event_type)
cbind(events, events2)

storm_sum$evtype2 <- "others"
storm_sum$evtype2[id] <- events2

storm_sum2 <- ddply(storm_sum[, -1], .(evtype2), colwise(sum))

op <- par(mar = c(8,4,2,1))
with(arrange(storm_sum2, desc(casualties), desc(totaldmg)),
     barplot(rbind(injuries, fatalities)[,1:20] / 1000, names.arg = evtype2[1:20], las = 3, col = 3:4,
             main = "Total Number of Casualties between 1996 and 2011 by Event Type",
             ylab = "Casualties ('000)"))
legend("topright", legend = c("Fatalities", "Injuries"), fill = 4:3, bty = "n")

with(arrange(storm_sum2, desc(totaldmg), desc(casualties)),
     barplot(rbind(propdmg, cropdmg)[,1:20] / 10^9, names.arg = evtype2[1:20], las = 3, col = 3:4,
             main = "Total Economic Damage between 1996 and 2011 by Event Type",
             ylab = "Economic damage (US$ billions)"))
legend("topright", legend = c("Crop", "Property"), fill = 4:3, bty = "n")
par(op)

library(knitr)
rm(list = ls())
knit2html("rr-report2.Rmd")

options(rpubs.upload.method = "internal")
write("options(rpubs.upload.method = \"internal\")", ".Rprofile")
