setwd("~/Github/datasciencecoursera/2-r-programming/")
df <- read.csv("hw1_data.csv")

# Q11
names(df)

# Q12
head(df, 2)

# Q13
nrow(df)

# Q14
tail(df, 2)

# Q15
df$Ozone[47]

# Q16
sum(is.na(df$Ozone))

# Q17
mean(df$Ozone, na.rm = T)

# Q18
mean(subset(df, Ozone > 31 & Temp > 90)$Solar.R)

# Q19
mean(subset(df, Month == 6)$Temp)

# Q20
max(subset(df, Month ==5)$Ozone, na.rm = T)
