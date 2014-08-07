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
colnames <- gsub("^t", "TimeDomain", colnames)
colnames <- gsub("^f", "FrequencyDomain", colnames)
colnames <- gsub("Acc", "Accelerometer", colnames)
colnames <- gsub("Gyro", "Gyroscope", colnames)
colnames <- gsub("Mag", "Magnitude", colnames)
colnames <- gsub("mean", "Mean", colnames)
colnames <- gsub("std", "StandardDeviation", colnames)
colnames <- gsub("X$", "XDirection", colnames)
colnames <- gsub("Y$", "YDirection", colnames)
colnames <- gsub("Z$", "ZDirection", colnames)
colnames <- gsub("BodyBody", "Body", colnames)

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
