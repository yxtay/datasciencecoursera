library(caret)

# Q1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
train <- subset(segmentationOriginal, Case == "Train", select = 3:119)
test <- subset(segmentationOriginal, Case == "Test", select = 3:119)

set.seed(125)
modelFit <- train(Class ~ ., method = "rpart", data = train)

plot(modelFit$finalModel, uniform = T)
text(modelFit$finalModel, use.n = T, all = T)

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
missClass(trainSA$chd, predict(modelFit, trainSA))
missClass(testSA$chd, predict(modelFit, testSA))

# Q5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train <- within(vowel.train, {y <- factor(y)})
vowel.test <- within(vowel.test, {y <- factor(y)})

set.seed(33833)
modelFit <- train(y ~ ., method = "rf", data = vowel.train, prox = T)
varImp(modelFit)
