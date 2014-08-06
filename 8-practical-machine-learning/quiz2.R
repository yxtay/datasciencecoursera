# Q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(ggplot2)
names(training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = Cement, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = BlastFurnaceSlag, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = FlyAsh, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = Water, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = Superplasticizer, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = CoarseAggregate, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = FineAggregate, data = training)
qplot(seq_along(CompressiveStrength), CompressiveStrength, col = Age, data = training)

# Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

with(training, hist(Superplasticizer))

# Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

predReq <- grep("^IL", names(training))
prePCA <- preProcess(training[predReq], method = "pca", thresh = 0.9)
prePCA$numComp

# Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

predReq <- grep("^IL", names(training))
prePCA <- preProcess(training[predReq], method = "pca", thresh = 0.8)
trainingPCA <- predict(prePCA, training[predReq])
testingPCA <- predict(prePCA, testing[predReq])
fit <- train(training$diagnosis ~ ., data = training[predReq], method = "glm")
fitPCA <- train(training$diagnosis ~ ., data = trainingPCA, method = "glm")

confusionMatrix(predict(fit, testing), testing$diagnosis)$overall[1]
confusionMatrix(predict(fitPCA, testingPCA), testing$diagnosis)$overall[1]
