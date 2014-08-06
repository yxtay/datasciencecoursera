setwd("~/GitHub/datasciencecoursera/3-getting-and-cleaning-data/")

# Q1
library(jsonlite)
URL <- "https://api.github.com/users/jtleek/repos"
jsonData <- fromJSON(URL)
subset(jsonData, name == "datasharing")$created_at

# Q4
URL <- "http://biostat.jhsph.edu/~jleek/contact.html"
con <- url(URL)
htmlCode <- readLines(con)
sapply(htmlCode[c(10,20,30,100)], nchar, USE.NAMES = F)
close(con)

# Q5
df <- read.fwf("getdata-wksst8110.for", widths = c(10,rep(c(9,4),4)), skip = 4)
sum(df[,4])
