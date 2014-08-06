setwd("~/Github/datasciencecoursera/3-getting-and-cleaning-data/")

# Q1
df <- read.csv("getdata-data-ss06hid.csv")
sum(df$VAL == 24, na.rm = T)

# Q3
library(xlsx)
dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx", header = T,
                sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm=T)

# Q4
library(XML)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xml <- xmlTreeParse("getdata-data-restaurants.xml", useInternalNodes = T)
rootNode <- xmlRoot(xml)
sum(xpathSApply(rootNode, "//zipcode", xmlValue) == 21231)

# Q5
library(data.table)
DT <- fread("getdata-data-ss06pid.csv")
DT[,mean(pwgtp15),by=SEX]
