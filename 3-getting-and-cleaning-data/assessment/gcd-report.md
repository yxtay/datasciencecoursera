Merging and Analysis of Smartphone Sensors Dataset
==================================================

## Reading the data

First, the file `run_analysis.R` should be in the same directory as the folder `UCI HAR Dataset`.
The datasets were contained in a few files the "UCI HAR Dataset" directory and subdirectories.


```r
if(!file.exists("UCI HAR Dataset")) unzip("getdata-projectfiles-UCI HAR Dataset.zip")
```

The names and directories of the files were first assigned to the `files` variable 
and rearranged such the the datasets for the test sets were first, followed by the train sets.


```r
files <- paste("UCI HAR Dataset", c("test", "train"),
               c("subject_test.txt", "subject_train.txt", "y_test.txt", "y_train.txt", "X_test.txt", "X_train.txt"),
               sep = "/")
print(files <- files[c(1,3,5,2,4,6)])
```

```
## [1] "UCI HAR Dataset/test/subject_test.txt"  
## [2] "UCI HAR Dataset/test/y_test.txt"        
## [3] "UCI HAR Dataset/test/X_test.txt"        
## [4] "UCI HAR Dataset/train/subject_train.txt"
## [5] "UCI HAR Dataset/train/y_train.txt"      
## [6] "UCI HAR Dataset/train/X_train.txt"
```

Next, the datasets were read in one go with the `lapply()` function.
The datasets were then combined using `rbind()` to create 2 data frames for test and train respectively.
These 2 resulting data frames were then combined using `cbind()` to create 1 data frame of all the data.


```r
df_list <- lapply(files, read.table)
df_test <- cbind(df_list[[1]], df_list[[2]], df_list[[3]])
df_train <- cbind(df_list[[4]], df_list[[5]], df_list[[6]])
df <- rbind(df_test, df_train)
rm(df_list, df_test, df_train)
```

# Selecting and naming variables

The next step was to select the required variables and give them appropriate labels.

There were 561 variables for the sensor data and it would be time consuming to name them from scratch.
Fortunately, there was a text file in the directory containing appropriate labels, so it was read in.


```r
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=F)[, 2]
names(df) <- c("Subject", "Activity", features)
head(names(df))
```

```
## [1] "Subject"           "Activity"          "tBodyAcc-mean()-X"
## [4] "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z" "tBodyAcc-std()-X"
```

At this stage, it would be wise to select only the required variables to reduce the size of the dataset.
The required variables were the measurements on the mean and standard deviation of each measurement.
After looking through the files describing the features,
it was apparent that those were the variables with "mean()" or "std()" in their names.
Hence, I used the `grep()` function to scan the variables for those string with a regular expression.
Columns 1 and 2 are necessary since they are the subject and activity variables respectively.
Only these variables were kept.


```r
colID <- grep("(mean|std)\\(\\)", names(df))
colID <- c(1:2, colID)
df <- df[, colID]
```

Next, the variable names were cleaned up so that they were appropriate and descriptive.
This was done by removing `()` and `-` from them using the `gsub()` function.
Short labels were also replaced with their original forms.
They were then asssigned to the data frame.


```r
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
head(names(df))
```

```
## [1] "Subject"                                               
## [2] "Activity"                                              
## [3] "TimeDomainBodyAccelerometerMeanXDirection"             
## [4] "TimeDomainBodyAccelerometerMeanYDirection"             
## [5] "TimeDomainBodyAccelerometerMeanZDirection"             
## [6] "TimeDomainBodyAccelerometerStandardDeviationXDirection"
```

## Labeling the activities

The data in the activity variable were given in integers 1 to 6 to represent 6 different activities.
They can be given appropriate labels with the code that followed.
The activity labels were first read from one of the descriptive files.
The activity variable was then converted to a factor with the activity labels.


```r
library(plyr)
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=F,
                              col.names = c("level", "label"))
print(activity_labels <- mutate(activity_labels, label = gsub("_", " ", tolower(label))))
```

```
##   level              label
## 1     1            walking
## 2     2   walking upstairs
## 3     3 walking downstairs
## 4     4            sitting
## 5     5           standing
## 6     6             laying
```

```r
df$Activity <- with(activity_labels, factor(df$Activity, levels = level, labels = label))
```

The structure of the dataset after all the merging and cleaning was shown as followed.


```r
str(df)
```

```
## 'data.frame':	10299 obs. of  68 variables:
##  $ Subject                                                        : int  2 2 2 2 2 2 2 2 2 2 ...
##  $ Activity                                                       : Factor w/ 6 levels "walking","walking upstairs",..: 5 5 5 5 5 5 5 5 5 5 ...
##  $ TimeDomainBodyAccelerometerMeanXDirection                      : num  0.257 0.286 0.275 0.27 0.275 ...
##  $ TimeDomainBodyAccelerometerMeanYDirection                      : num  -0.0233 -0.0132 -0.0261 -0.0326 -0.0278 ...
##  $ TimeDomainBodyAccelerometerMeanZDirection                      : num  -0.0147 -0.1191 -0.1182 -0.1175 -0.1295 ...
##  $ TimeDomainBodyAccelerometerStandardDeviationXDirection         : num  -0.938 -0.975 -0.994 -0.995 -0.994 ...
##  $ TimeDomainBodyAccelerometerStandardDeviationYDirection         : num  -0.92 -0.967 -0.97 -0.973 -0.967 ...
##  $ TimeDomainBodyAccelerometerStandardDeviationZDirection         : num  -0.668 -0.945 -0.963 -0.967 -0.978 ...
##  $ TimeDomainGravityAccelerometerMeanXDirection                   : num  0.936 0.927 0.93 0.929 0.927 ...
##  $ TimeDomainGravityAccelerometerMeanYDirection                   : num  -0.283 -0.289 -0.288 -0.293 -0.303 ...
##  $ TimeDomainGravityAccelerometerMeanZDirection                   : num  0.115 0.153 0.146 0.143 0.138 ...
##  $ TimeDomainGravityAccelerometerStandardDeviationXDirection      : num  -0.925 -0.989 -0.996 -0.993 -0.996 ...
##  $ TimeDomainGravityAccelerometerStandardDeviationYDirection      : num  -0.937 -0.984 -0.988 -0.97 -0.971 ...
##  $ TimeDomainGravityAccelerometerStandardDeviationZDirection      : num  -0.564 -0.965 -0.982 -0.992 -0.968 ...
##  $ TimeDomainBodyAccelerometerJerkMeanXDirection                  : num  0.072 0.0702 0.0694 0.0749 0.0784 ...
##  $ TimeDomainBodyAccelerometerJerkMeanYDirection                  : num  0.04575 -0.01788 -0.00491 0.03227 0.02228 ...
##  $ TimeDomainBodyAccelerometerJerkMeanZDirection                  : num  -0.10604 -0.00172 -0.01367 0.01214 0.00275 ...
##  $ TimeDomainBodyAccelerometerJerkStandardDeviationXDirection     : num  -0.907 -0.949 -0.991 -0.991 -0.992 ...
##  $ TimeDomainBodyAccelerometerJerkStandardDeviationYDirection     : num  -0.938 -0.973 -0.971 -0.973 -0.979 ...
##  $ TimeDomainBodyAccelerometerJerkStandardDeviationZDirection     : num  -0.936 -0.978 -0.973 -0.976 -0.987 ...
##  $ TimeDomainBodyGyroscopeMeanXDirection                          : num  0.11998 -0.00155 -0.04821 -0.05664 -0.05999 ...
##  $ TimeDomainBodyGyroscopeMeanYDirection                          : num  -0.0918 -0.1873 -0.1663 -0.126 -0.0847 ...
##  $ TimeDomainBodyGyroscopeMeanZDirection                          : num  0.1896 0.1807 0.1542 0.1183 0.0787 ...
##  $ TimeDomainBodyGyroscopeStandardDeviationXDirection             : num  -0.883 -0.926 -0.973 -0.968 -0.975 ...
##  $ TimeDomainBodyGyroscopeStandardDeviationYDirection             : num  -0.816 -0.93 -0.979 -0.975 -0.978 ...
##  $ TimeDomainBodyGyroscopeStandardDeviationZDirection             : num  -0.941 -0.968 -0.976 -0.963 -0.968 ...
##  $ TimeDomainBodyGyroscopeJerkMeanXDirection                      : num  -0.2049 -0.1387 -0.0978 -0.1022 -0.0918 ...
##  $ TimeDomainBodyGyroscopeJerkMeanYDirection                      : num  -0.1745 -0.0258 -0.0342 -0.0447 -0.029 ...
##  $ TimeDomainBodyGyroscopeJerkMeanZDirection                      : num  -0.0934 -0.0714 -0.06 -0.0534 -0.0612 ...
##  $ TimeDomainBodyGyroscopeJerkStandardDeviationXDirection         : num  -0.901 -0.962 -0.984 -0.984 -0.988 ...
##  $ TimeDomainBodyGyroscopeJerkStandardDeviationYDirection         : num  -0.911 -0.956 -0.988 -0.99 -0.992 ...
##  $ TimeDomainBodyGyroscopeJerkStandardDeviationZDirection         : num  -0.939 -0.981 -0.976 -0.981 -0.982 ...
##  $ TimeDomainBodyAccelerometerMagnitudeMean                       : num  -0.867 -0.969 -0.976 -0.974 -0.976 ...
##  $ TimeDomainBodyAccelerometerMagnitudeStandardDeviation          : num  -0.705 -0.954 -0.979 -0.977 -0.977 ...
##  $ TimeDomainGravityAccelerometerMagnitudeMean                    : num  -0.867 -0.969 -0.976 -0.974 -0.976 ...
##  $ TimeDomainGravityAccelerometerMagnitudeStandardDeviation       : num  -0.705 -0.954 -0.979 -0.977 -0.977 ...
##  $ TimeDomainBodyAccelerometerJerkMagnitudeMean                   : num  -0.93 -0.974 -0.982 -0.983 -0.987 ...
##  $ TimeDomainBodyAccelerometerJerkMagnitudeStandardDeviation      : num  -0.896 -0.941 -0.971 -0.975 -0.989 ...
##  $ TimeDomainBodyGyroscopeMagnitudeMean                           : num  -0.796 -0.898 -0.939 -0.947 -0.957 ...
##  $ TimeDomainBodyGyroscopeMagnitudeStandardDeviation              : num  -0.762 -0.911 -0.972 -0.97 -0.969 ...
##  $ TimeDomainBodyGyroscopeJerkMagnitudeMean                       : num  -0.925 -0.973 -0.987 -0.989 -0.99 ...
##  $ TimeDomainBodyGyroscopeJerkMagnitudeStandardDeviation          : num  -0.894 -0.944 -0.984 -0.986 -0.99 ...
##  $ FrequencyDomainBodyAccelerometerMeanXDirection                 : num  -0.919 -0.961 -0.992 -0.993 -0.992 ...
##  $ FrequencyDomainBodyAccelerometerMeanYDirection                 : num  -0.918 -0.964 -0.965 -0.968 -0.969 ...
##  $ FrequencyDomainBodyAccelerometerMeanZDirection                 : num  -0.789 -0.957 -0.967 -0.967 -0.98 ...
##  $ FrequencyDomainBodyAccelerometerStandardDeviationXDirection    : num  -0.948 -0.984 -0.995 -0.996 -0.995 ...
##  $ FrequencyDomainBodyAccelerometerStandardDeviationYDirection    : num  -0.925 -0.97 -0.974 -0.977 -0.967 ...
##  $ FrequencyDomainBodyAccelerometerStandardDeviationZDirection    : num  -0.636 -0.942 -0.962 -0.969 -0.978 ...
##  $ FrequencyDomainBodyAccelerometerJerkMeanXDirection             : num  -0.9 -0.944 -0.991 -0.991 -0.991 ...
##  $ FrequencyDomainBodyAccelerometerJerkMeanYDirection             : num  -0.937 -0.969 -0.973 -0.972 -0.98 ...
##  $ FrequencyDomainBodyAccelerometerJerkMeanZDirection             : num  -0.924 -0.973 -0.972 -0.97 -0.983 ...
##  $ FrequencyDomainBodyAccelerometerJerkStandardDeviationXDirection: num  -0.924 -0.962 -0.992 -0.992 -0.994 ...
##  $ FrequencyDomainBodyAccelerometerJerkStandardDeviationYDirection: num  -0.943 -0.98 -0.971 -0.975 -0.979 ...
##  $ FrequencyDomainBodyAccelerometerJerkStandardDeviationZDirection: num  -0.948 -0.981 -0.972 -0.981 -0.989 ...
##  $ FrequencyDomainBodyGyroscopeMeanXDirection                     : num  -0.824 -0.923 -0.973 -0.972 -0.976 ...
##  $ FrequencyDomainBodyGyroscopeMeanYDirection                     : num  -0.808 -0.926 -0.981 -0.981 -0.98 ...
##  $ FrequencyDomainBodyGyroscopeMeanZDirection                     : num  -0.918 -0.968 -0.972 -0.967 -0.969 ...
##  $ FrequencyDomainBodyGyroscopeStandardDeviationXDirection        : num  -0.903 -0.927 -0.973 -0.967 -0.974 ...
##  $ FrequencyDomainBodyGyroscopeStandardDeviationYDirection        : num  -0.823 -0.932 -0.977 -0.972 -0.977 ...
##  $ FrequencyDomainBodyGyroscopeStandardDeviationZDirection        : num  -0.956 -0.97 -0.979 -0.965 -0.97 ...
##  $ FrequencyDomainBodyAccelerometerMagnitudeMean                  : num  -0.791 -0.954 -0.976 -0.973 -0.978 ...
##  $ FrequencyDomainBodyAccelerometerMagnitudeStandardDeviation     : num  -0.711 -0.96 -0.984 -0.982 -0.979 ...
##  $ FrequencyDomainBodyAccelerometerJerkMagnitudeMean              : num  -0.895 -0.945 -0.971 -0.972 -0.987 ...
##  $ FrequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation : num  -0.896 -0.934 -0.97 -0.978 -0.99 ...
##  $ FrequencyDomainBodyGyroscopeMagnitudeMean                      : num  -0.771 -0.924 -0.975 -0.976 -0.977 ...
##  $ FrequencyDomainBodyGyroscopeMagnitudeStandardDeviation         : num  -0.797 -0.917 -0.974 -0.971 -0.97 ...
##  $ FrequencyDomainBodyGyroscopeJerkMagnitudeMean                  : num  -0.89 -0.952 -0.986 -0.986 -0.99 ...
##  $ FrequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation     : num  -0.907 -0.938 -0.983 -0.986 -0.991 ...
```

## Computing the mean of each variable by activity and subject

This can be achieved using the `ddply()` function 
together with the `colwise` function from the `plyr` package.
The resulting data frame was saved in "tidy_data.txt".
It can be read into R using `read.table()`.


```r
df_mean <- ddply(df, .(Activity, Subject), colwise(mean))
write.table(df_mean, "tidy_data.txt", row.names=F)
```
