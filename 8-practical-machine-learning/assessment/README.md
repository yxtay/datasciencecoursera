

Practical Machine Learning Assignment
=====================================

## Introduction

In this project, the goal was to use data from accelerometers on the belt, forearm, arm, and dumbell
of 6 participants to predict the manner in which they did the exercise. 
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

## Data


```r
train <- read.csv("pml-training.csv", na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv("pml-testing.csv", , na.strings = c("", "NA"), stringsAsFactors = F)
```

The data was obtained from the [Human Activity Recognition](http://groupware.les.inf.puc-rio.br/har) 
reasearch by Groupware Technologies.
The training dataset containted 19622 observations with 160 variables.
The test dataset had 20 observations and the same number of variables.
The variable of interest was `classe`, which classified the manner the participants performed
the dumbbell bicep curl. It took on 5 values: A, B, C, D and E.

## Dimensionality Reduction


```r
varNA <- apply(sapply(train, is.na), 2, sum)
train_sub <- subset(train, select = (varNA == 0))[, -(1:7)]
test_sub <- subset(test, select = (varNA == 0))[, -(1:7)]
```

On reviewing the datasets, it was observed that many of the measurements had missing values
for the majority of the obsersvations. Hence, it would be appropriate to remove such variables 
since they would not be useful for prediction. 
Other variables such as name of participants, time, etc. would also not be useful and were removed. 
The resulting datasets had 53 variables.

An inspection of the correlation matrix between the variables showed that the R-squared
between 11 paris of variables were more than 0.80.
The high correlation between a few of the variables was evident in the correlation plot below.


```r
library(corrplot)
corm <- cor(train_sub[, -53])
sort(corm[upper.tri(corm)] ^ 2, decreasing = T)[1:12]
corrplot(corm, order = "hclust")
```

<img src="figure/corrplot.png" title="plot of chunk corrplot" alt="plot of chunk corrplot" style="display: block; margin: auto;" />

```
##  [1] 0.9841 0.9622 0.9583 0.9505 0.9326 0.8712 0.8706 0.8608 0.8554 0.8431
## [11] 0.8363 0.7958
```

Further reduction in the dimensionality can be achieved using principle component analysis.
Principle component analysis was performed with a threshold of 99% cumulative variance retained.
The same transformation was also performed on the test dataset.


```r
library(caret)
preProc <- preProcess(train_sub[, -53], method = "pca", thresh = 0.99)
train_proc <- cbind(predict(preProc, train_sub[, -53]), classe = train_sub$classe)
test_proc <- cbind(predict(preProc, test_sub[, -53]), problem_id = test_sub$problem_id)
```

## Model Building and Validation

I chose to use the Random Forests method due to its performance in predictions.
The training data was split 60/40 to set aside 40% of it for validation later.
The model was built on 60% of the training dataset.
The model building took more than an hour, probably due to the low memory available on my laptop.


```r
library(randomForest)
set.seed(0)
inTrain <- createDataPartition(train_sub$classe, p = 0.6, list = F)
system.time(modelFitPCA <- train(classe ~ ., data = train_proc[inTrain, ],
                                 method = "rf", trControl = trainControl(method = "cv"), prox = T))
```

```
##    user  system elapsed 
##   384.2     2.2  3269.1
```

After the model has been constructed, it would be appropriate to get an idea of its performance.
This could be evaluated through the prediction error.
The prediction error could be estimated by the out-of-sample error.
This could be computed by performing predictions on the validation dataset with the model
and checking the predictions against their true outcomes.
The in-sample error was not used as it would be biased due to potential overfitting.


```r
outError <- confusionMatrix(predict(modelFitPCA, train_proc[-inTrain, ]), train_proc[-inTrain, ]$classe)
outError
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2222   31    1    4    0
##          B    4 1471   12    2    5
##          C    1   12 1344   53   10
##          D    3    0   10 1225    7
##          E    2    4    1    2 1420
## 
## Overall Statistics
##                                         
##                Accuracy : 0.979         
##                  95% CI : (0.976, 0.982)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : < 2e-16       
##                                         
##                   Kappa : 0.974         
##  Mcnemar's Test P-Value : 4.89e-10      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.996    0.969    0.982    0.953    0.985
## Specificity             0.994    0.996    0.988    0.997    0.999
## Pos Pred Value          0.984    0.985    0.946    0.984    0.994
## Neg Pred Value          0.998    0.993    0.996    0.991    0.997
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.283    0.187    0.171    0.156    0.181
## Detection Prevalence    0.288    0.190    0.181    0.159    0.182
## Balanced Accuracy       0.995    0.983    0.985    0.975    0.992
```

For the Random Forest model, the out-of-sample error was **97.91%**.
This could be considered as quite a well performing prediction.
The model was then applied onto the test dataset. The outcomes were as followed
and written individually into seperate files as instructed.


```r
prediction <- predict(modelFitPCA, test_proc)
names(prediction) <- 1:20
prediction
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
setwd("predictions")
pml_write_files(prediction)
setwd("..")
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  A  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
