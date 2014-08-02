setwd("~/Github/datasciencecoursera/Getting and Cleaning Data")

# Q1

data <- read.csv("getdata-data-ss06hid.csv")
strsplit(names(data), "wgtp")[[123]]

# Q2

data <- read.csv("getdata-data-GDP.csv", skip = 4, nrows = 190, 
                 stringsAsFactors = F, 
                 colClasses = c("character", "integer", "NULL", "character", "character", rep("NULL", 5)),
                 col.names = c("countrycode", "rank", NA, "country", "gdp", rep(NA, 5)))
mean(as.numeric(gsub(",", "", data$gdp)))

# Q3

length(grep("^United", data$country))

# Q4

data <- read.csv("getdata-data-EDSTATS_Country.csv")
sum(grepl("[Ff]iscal year end: June", df2$Special.Notes))

# Q5

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

library(lubridate)
sum(year(sampleTimes) == 2012)
sum(year(sampleTimes) == 2012 & weekdays(sampleTimes) == "Monday")
