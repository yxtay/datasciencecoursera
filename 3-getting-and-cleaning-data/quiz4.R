setwd("~/Github/datasciencecoursera/3. Getting and Cleaning Data/")

# Q1
df <- read.csv("getdata-data-ss06hid.csv")
strsplit(names(df), "wgtp")[[123]]

# Q2
df <- read.csv("getdata-data-GDP.csv", skip = 4, nrows = 190, 
               stringsAsFactors = F, 
               colClasses = c("character", "integer", "NULL", "character", "character", rep("NULL", 5)),
               col.names = c("CountryCode", "Rank", NA, "Country", "GDP", rep(NA, 5)))
df <- within(df, GDP <- as.numeric(gsub(",", "", GDP)))
mean(df$GDP)

# Q3
length(grep("^United", df$Country))

# Q4
df <- read.csv("getdata-data-EDSTATS_Country.csv")
sum(grepl("[Ff]iscal year end: June", df$Special.Notes))

# Q5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

library(lubridate)
sum(year(sampleTimes) == 2012)
sum(year(sampleTimes) == 2012 & weekdays(sampleTimes) == "Monday")
