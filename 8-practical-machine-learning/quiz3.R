library(caret)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

train <- subset(segmentationOriginal, Case == "Train", select = -(1:2))
test <- subset(segmentationOriginal, Case == "Test", select = -(1:2))
set.seed(125)
modelFit <- train(Class ~ ., method = "rpart", data = train)
library(rattle)
fancyRpartPlot(modelFit$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2: PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100: WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100: PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2: NA

# Q3
library(pgmm)
data(olive)
olive = olive[,-1]

modelFit <- train(Area ~ ., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modelFit, newdata)

# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart

modelFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                  method = "glm", family = "binomial", data = trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, predict(modelFit, testSA))
missClass(trainSA$chd, predict(modelFit, trainSA))

# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train <- within(vowel.train, {y <- factor(y)})
vowel.test <- within(vowel.test, {y <- factor(y)})
set.seed(33833)
system.time(modelFit <- train(y ~ ., method = "rf", data = vowel.train, prox = T))
varImp(modelFit)

stopCluster(cl)
rm(list = ls())
