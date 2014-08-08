setwd("~/GitHub/datasciencecoursera/3-getting-and-cleaning-data/assessment/")

# loading required package
library(plyr)

# unzipping files
if(!file.exists("UCI HAR Dataset")) unzip("getdata-projectfiles-UCI HAR Dataset.zip")

# reading the files
files <- paste("UCI HAR Dataset", c("test", "train"),
               c("subject_test.txt", "subject_train.txt", "y_test.txt", "y_train.txt", "X_test.txt", "X_train.txt"),
               sep = "/")
files <- files[c(1,3,5,2,4,6)]
df_list <- lapply(files, read.table)

# merging the data frames
df_test <- cbind(df_list[[1]], df_list[[2]], df_list[[3]])
df_train <- cbind(df_list[[4]], df_list[[5]], df_list[[6]])
df <- rbind(df_test, df_train)

# obtaining variable names
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=F)[, 2]
names(df) <- c("Subject", "Activity", features)

# selecting variables containing mean() or std()
colID <- grep("(mean|std)\\(\\)", names(df))
colID <- c(1:2, colID)
df <- df[, colID]

# naming the variables appropriately
colnames <- names(df)
colnames <- gsub("\\(\\)", "", colnames)
colnames <- gsub("-", "", colnames)
colnames <- gsub("^t", "Time", colnames)
colnames <- gsub("^f", "Frequency", colnames)
colnames <- gsub("Acc", "Accelerometer", colnames)
colnames <- gsub("Gyro", "Gyroscope", colnames)
colnames <- gsub("Mag", "Magnitude", colnames)
colnames <- gsub("mean", "Mean", colnames)
colnames <- gsub("std", "StandardDeviation", colnames)
colnames <- gsub("([X-Z]$)", "\\1Direction", colnames)
colnames <- gsub("(Body)\\1", "\\1", colnames)

names(df) <- colnames

# labeling the activity descriptively
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=F,
                              col.names = c("level", "label"))
activity_labels <- mutate(activity_labels, label = gsub("_", " ", tolower(label)))
df$Activity <- with(activity_labels, factor(df$Activity, levels = level, labels = label))

# computing the mean of the variables by activity and subject
df_mean <- ddply(df, .(Activity, Subject), colwise(mean))

# outputing the data frame into a text file
write.table(df_mean, "tidy_data.txt", row.names=F)

# producing codebook.txt
coldescript <- gsub("([A-Z])", " \\1", colnames)
coldescript <- gsub("^ ", "", coldescript)
coldescript <- gsub("(Time|Frequency) (.*) (Mean|Standard Deviation)", "\\3 \\2 \\1 Domain Signal", coldescript)
coldescript <- gsub("(^Mean|^Standard)", "Mean of \\1", coldescript)
coldescript <- gsub("(Deviation)", "\\1 of", coldescript)
coldescript <- gsub("([X-Z])", "in the \\1", coldescript)
coldescript <- gsub("(Time.*$)", "\\1 in seconds", coldescript)
coldescript <- gsub("(Frequency.*$)", "\\1 in hertz", coldescript)

colnames <- paste0("**", colnames, "**")
coldescript <- paste("-", coldescript)
codebook <- c(rbind(colnames, "", coldescript, ""))
write(codebook, "codebook.txt")

library(knitr)
knit2html("gcd-report.Rmd")
knit2html("codebook.Rmd")
