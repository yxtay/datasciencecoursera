setwd("~/Github/datasciencecoursera/Practical Machine Learning")

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Q1

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

library(caret)
vowel.train <- within(vowel.train, {y <- factor(y)})
vowel.test <- within(vowel.test, {y <- factor(y)})
set.seed(33833)
modelRF <- train(y ~ ., data = vowel.train, method = "rf")
modelGBM <- train(y ~ ., data = vowel.train, method = "gbm")

predictRF <- predict(modelRF, vowel.test)
predictGBM <- predict(modelGBM, vowel.test)
agree <- predictRF == predictGBM
confusionMatrix(predictRF, vowel.test$y)$overall
confusionMatrix(predictGBM, vowel.test$y)$overall
confusionMatrix(predictRF[agree], vowel.test$y[agree])$overall

rm(list = ls()[ls() != "cl"])

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
combDFtraining <- data.frame(predictRFtraining, predictGBMtraining, predictLDAtraining, diagnosis = training$diagnosis)
modelComb <- train(diagnosis ~ ., data = combDFtraining, method = "gam")

predictRFtesting <- predict(modelRF, testing)
predictGBMtesting <- predict(modelGBM, testing)
predictLDAtesting <- predict(modelLDA, testing)
combDFtesting <- data.frame(predictRFtesting, predictGBMtesting, predictLDAtesting, diagnosis = testing$diagnosis)
modelComb <- train(diagnosis ~ ., data = combDFtesting, method = "gam")

1 - confusionMatrix(predict(modelComb, combDFtesting), testing$diagnosis)$overall[1]

rm(list = ls()[ls() != "cl"])

# Q3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seet(233)
modelLASSO <- train(CompressiveStrength ~ ., data = training, method = "lasso")
modelLASSO <- enet(as.matrix(training[, names(training) != "CompressiveStrength"]), training$CompressiveStrength, lambda = 0)
plot(modelLASSO, xvar = "step")

rm(list = ls()[ls() != "cl"])

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

rm(list = ls()[ls() != "cl"])

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
