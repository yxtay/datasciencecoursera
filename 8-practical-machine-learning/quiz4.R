setwd("~/Github/datasciencecoursera/8-practical-machine-learning/")

library(caret)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train <- within(vowel.train, {y <- factor(y)})
vowel.test <- within(vowel.test, {y <- factor(y)})
set.seed(33833)
modelRF <- train(y ~ ., data = vowel.train, method = "rf")
modelGBM <- train(y ~ ., data = vowel.train, method = "gbm")

predictRF <- predict(modelRF, vowel.test)
predictGBM <- predict(modelGBM, vowel.test)
agree <- predictRF == predictGBM
confusionMatrix(predictRF, vowel.test$y)$overall[1]
confusionMatrix(predictGBM, vowel.test$y)$overall[1]
confusionMatrix(predictRF[agree], vowel.test$y[agree])$overall[1]

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modelRF <- train(diagnosis ~ ., data = training, method = "rf")
modelGBM <- train(diagnosis ~ ., data = training, method = "gbm")
modelLDA <- train(diagnosis ~ ., data = training, method = "lda")

predictRFtraining <- predict(modelRF, training)
predictGBMtraining <- predict(modelGBM, training)
predictLDAtraining <- predict(modelLDA, training)
combDFtraining <- data.frame(predictRF = predictRFtraining, predictGBM = predictGBMtraining, predictLDA = predictLDAtraining, diagnosis = training$diagnosis)
modelComb <- train(diagnosis ~ ., data = combDFtraining, method = "gam")

predictRFtesting <- predict(modelRF, testing)
predictGBMtesting <- predict(modelGBM, testing)
predictLDAtesting <- predict(modelLDA, testing)
combDFtesting <- data.frame(predictRF = predictRFtesting, predictGBM = predictGBMtesting, predictLDA = predictLDAtesting)

confusionMatrix(predictRFtesting, testing$diagnosis)$overall[1]
confusionMatrix(predictGBMtesting, testing$diagnosis)$overall[1]
confusionMatrix(predictLDAtesting, testing$diagnosis)$overall[1]
1 - confusionMatrix(predict(modelComb, combDFtesting), testing$diagnosis)$overall[1]

# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modelLASSO <- train(CompressiveStrength ~ ., training, method = "lasso")
plot(modelLASSO$finalModel, xvar = "penalty")

# Q4
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)

library(forecast)
modelBATS <- bats(tstrain)
fcast <- forecast(modelBATS, h = length(tstest), level = 95)
with(fcast, mean(tstest > lower & tstest < upper))

# Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)
modelSVD <- svm(CompressiveStrength ~ ., data = training)
predictSVD <- predict(modelSVD, testing)
sqrt(mean((predictSVD - testing$CompressiveStrength) ^ 2))

stopCluster(cl)
rm(list = ls())
