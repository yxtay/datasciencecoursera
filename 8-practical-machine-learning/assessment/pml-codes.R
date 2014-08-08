rm(list=ls())
setwd("~/Github/datasciencecoursera/8-practical-machine-learning/assessment/")

library(caret); library(randomForest); library(gbm)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

train <- read.csv("pml-training.csv", na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv("pml-testing.csv", , na.strings = c("", "NA"), stringsAsFactors = F)
train <- within(train, {classe <- factor(classe)})

sumNA <- apply(sapply(train, is.na), 2, sum)
table(sumNA)
names(train)[1:7]
train_sub <- subset(train, select = (sumNA == 0))[, -(1:7)]
test_sub <- subset(test, select = (sumNA == 0))[, -(1:7)]
str(train_sub)

print(grid <- seq(1, 52, by = 3))

set.seed(1)
seeds <- c(replicate(2, sample.int(1000, 52), simplify = F), sample.int(1000, 1))
modelRF1 <- train(classe ~ ., data = train_sub, method = "rf",
                 trControl = trainControl(method = "cv", number = 2, seeds = seeds),
                 tuneGrid = data.frame(mtry = seq(1, 52, by = 3)))
modelRF1$times$everything
plot(modelRF1$finalModel, log = "y")

plot(modelRF1)
gridrng <- with(modelRF1$results, range(mtry[Accuracy > 0.99]))
print(grid <- gridrng[1]:gridrng[2])

set.seed(2)
seeds <- c(replicate(5, sample.int(1000, length(grid)), simplify = F), sample.int(1000, 1))
modelRF2 <- train(classe ~ ., data = train_sub, method = "rf", ntree = 200,
                  trControl = trainControl(method = "cv", number = 5, seeds = seeds),
                  tuneGrid = data.frame(mtry = grid))
plot(modelRF2)
print(best <- modelRF2$bestTune$mtry)

set.seed(3)
seeds <- as.list(sample.int(1000, 11))
modelRF3 <- train(classe ~ ., data = train_sub, method = "rf",
                  trControl = trainControl(method = "cv", seeds = seeds),
                  tuneGrid = data.frame(mtry = best))
modelRF3$finalModel
print(results <- modelRF3$results)
1 - results$Accuracy

predRF1 <- predict(modelRF1, test_sub)
predRF2 <- predict(modelRF2, test_sub)
predRF3 <- predict(modelRF3, test_sub)
data.frame(predRF1, predRF2, predRF3)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
setwd("predictions")
pml_wrtie_files(predRF3)
setwd("..")
stopCluster(cl)

library(knitr)
knit2html("pml-report.Rmd")
