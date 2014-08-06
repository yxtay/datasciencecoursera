setwd("~/GitHub/datasciencecoursera/3. Getting and Cleaning Data/")

# Q1
df <- read.csv("getdata-data-ss06hid.csv")
agricultureLogical <- with(df, ACR == 3 & AGS == 6)
which(agricultureLogical)

# Q2
library(jpeg)
jpegData <- readJPEG("getdata-jeff.jpg", native = T)
quantile(jpegData, c(0.3,0.8))

# Q3
df <- read.csv("getdata-data-GDP.csv", skip = 4, nrows = 190, 
               stringsAsFactors = F, 
               colClasses = c("character", "integer", "NULL", "character", "character", rep("NULL", 5)),
               col.names = c("CountryCode", "Rank", NA, "Country", "GDP", rep(NA, 5)))
df <- within(df, GDP <- as.numeric(gsub(",", "", GDP)))
df2 <- read.csv("getdata-data-EDSTATS_Country.csv", stringsAsFactors = F)
dfmerge <- merge(df, df2, by = "CountryCode")
nrow(dfmerge)
with(dfmerge, Country[order(Rank, decreasing = T)[13]])

# Q4
with(dfmerge, tapply(Rank, Income.Group, mean))

# Q5
library(Hmisc)
dfmerge <- within(dfmerge, {RankGroup <- cut2(Rank, g = 5)})
with(dfmerge, table(Income.Group, RankGroup))
