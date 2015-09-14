### International Rugby game prediction model ###

# load libraries
library(caret)
library(randomForest)
library(ggplot2)
library(reshape2)

# load data
setwd("/Users/arnupretorius/Google Drive/Personal/rugbyPrediction")
data <- read.csv("RWCData.csv")

# split data
set.seed(123)
trainIndex <- createDataPartition(data[,1], p = .8,
                                  list = FALSE,
                                  times = 1)
train <- data[trainIndex,]
test  <- data[-trainIndex,]

# Correlation heat map of the training data
trainCor <- cbind(as.numeric(train[,1]), as.numeric(train[,2]), as.numeric(train[,3]), train[-c(1,2,3)])
title <- "Rugby training data correlation heat map"
corp <- qplot(x=Var1, y=Var2, data=melt(cor(trainCor, use="p")), fill=value, geom="tile") +
      scale_fill_gradient2(limits=c(-1, 1))
corp <- corp + theme(axis.title.x=element_blank(), axis.text.x=element_blank()
                 , axis.ticks=element_blank())
corp <- corp + ggtitle(title)
corp

# train classifiers
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

# Model tuning method
rfGrid <-  expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45, 50))

rfRulesGrid <- expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45),
                           maxdepth= c(2,3,4))

etGrid <- expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45),
                      numRandomCuts = c(1,2,3))

rrfGrid <- expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45),
                       coefReg = c(0.01,0.505,1),
                       coefImp = c(0, 0.5, 1))

grrfGrid <- expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45),
                        coefReg = c(0.01,0.505,1))

wsrfGrid <- expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 44))

fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)

# Random Forests
rf <- train(x=train[,-1], y=train[,1], method="rf", ntree=1000,trControl=fitControl, tuneGrid=rfGrid, importance=TRUE)

# Random Forests with pruned trees
rfRules <- train(x=train[,-1], y=train[,1], method="rfRules", ntree=1000, trControl=fitControl, tuneGrid=rfRulesGrid)

# Oblique random forests 
orflog <- train(winner~., data=train, method="ORFlog", ntree=1000, trControl=fitControl, tuneGrid=rfGrid)
orfridge <- train(winner~., data=train, method="ORFridge", ntree=1000, trControl=fitControl, tuneGrid=rfGrid)
orfsvm <- train(winner~., data=train, method="ORFsvm", ntree=1000, trControl=fitControl, tuneGrid=rfGrid)
orfpls <- train(winner~., data=train, method="ORFpls", ntree=1000, trControl=fitControl, tuneGrid=rfGrid)

# Regularized random forests
rrf <- train(x=train[,-1], y=train[,1], method="RRF", ntree=1000, trControl=fitControl, tuneGrid=rrfGrid)
grrf <- train(x=train[,-1], y=train[,1], method="RRFglobal", ntree=1000, trControl=fitControl, tuneGrid=grrfGrid)

# Extremely Randomized forests
et <- train(winner~., data=train, method="extraTrees", ntree=1000, trControl=fitControl, tuneGrid=etGrid)

# Weighted Subspace Random Forests
wsrf <- train(x=train[,-1], y=train[,1], method="wsrf", ntree=1000, trControl=fitControl, tuneGrid=wsrfGrid)

modelList <- list(rf=rf, rfRules=rfRules, orflog=orflog, orfridge=orfridge,
                  orfsvm=orfsvm, orfpls=orfpls, rrf=rrf, grrf=grrf, et=et, wsrf=wsrf)

# Save all the models
saveRDS(modelList, file = "modelList.rda")

# Stop parallel processing cluster
stopImplicitCluster()

# Read in all the trained models  
modelList <- readRDS(file="modelList.rda")

# plot tuning parameters effects
library(gridExtra)
tuningPlotList <- list()
oobPlotList <- list()
predsList <- list()
confuseMatList <- list()
ROCcurveList <- list()
accVec <- NULL
OOBcount <- 1
for(i in 1:length(modelList)){
      tuningPlotList[[i]] <- plot(modelList[[i]])
      best <- modelList[[i]]$finalModel
      predsList[[i]] <- predict(modelList[[i]], test)
      
      # Plots ROC curves
      if(i != 2){
            roc.curve <- function(s , print=FALSE){
                  preds <- predict(modelList[[i]], test, type="prob")[,2]
                  Y <- test$winner
                  Y <- as.numeric(Y)-1
                  Ps=(preds>s)*1
                  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
                  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
                  if(print==TRUE){
                        print(table(Observed=Y,Predicted=Ps))
                  }
                  vect=c(FP,TP)
                  names(vect)=c("FPR","TPR")
                  return(vect)
            }
            ROC.curve <- Vectorize(roc.curve)
            M.ROC <- ROC.curve(seq(0,1,by=.01))
            ROCcurveList[[i]] <- t(M.ROC) 
      }
      
      confuseMatList[[i]] <- confusionMatrix(predsList[[i]], test$winner)
      accVec[i] <- sum(as.numeric(predsList[[i]] == test$winner))/nrow(test)
      if(length(best$err.rate[,1]) != 0){
            print(i)
            oobPlotList[[OOBcount]] <- ggplot(data.frame(best$err.rate), aes(x=1:1000, y=OOB)) + geom_line(colour="red") +
                  ggtitle("Out-of-bag Error vs Number of trees") + xlab("Number of trees") + ylab("OOB Error")
            OOBcount <- OOBcount + 1
      } 
      
}
grid.arrange(tuningPlotList[[1]], tuningPlotList[[2]], tuningPlotList[[3]], tuningPlotList[[4]], tuningPlotList[[5]]
             , tuningPlotList[[6]], tuningPlotList[[7]], tuningPlotList[[8]], tuningPlotList[[9]], tuningPlotList[[10]], nrow=2, ncol=5)
grid.arrange(oobPlotList[[1]], oobPlotList[[2]], oobPlotList[[3]], nrow=1, ncol=3)

resamps <- resamples(list(rf=rf, rfRules=rfRules, orflog=orflog,
                          orfridge=orfridge, orfsvm=orfsvm, orfpls=orfpls,
                          rrf=rrf, grrf=grrf, et=et, wsrf=wsrf))

bwplot(resamps)
splom(resamps)

rocp <- ggplot(data.frame(ROCcurveList[[1]]), aes(ROCcurveList[[1]][,1],ROCcurveList[[1]][,2], color="green")) + geom_line()
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[3]]), aes(ROCcurveList[[3]][,1], ROCcurveList[[3]][,2], color="blue"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[4]]), aes(ROCcurveList[[4]][,1], ROCcurveList[[4]][,2], color="yellow"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[5]]), aes(ROCcurveList[[5]][,1], ROCcurveList[[5]][,2], color="orange"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[6]]), aes(ROCcurveList[[6]][,1], ROCcurveList[[6]][,2], color="red"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[7]]), aes(ROCcurveList[[7]][,1], ROCcurveList[[7]][,2], color="pink"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[8]]), aes(ROCcurveList[[8]][,1], ROCcurveList[[8]][,2], color="cyan"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[9]]), aes(ROCcurveList[[9]][,1], ROCcurveList[[9]][,2], color="purple"))
rocp <- rocp + geom_line(data=data.frame(ROCcurveList[[10]]), aes(ROCcurveList[[10]][,1], ROCcurveList[[10]][,2], color="magenta"))
rocp <- rocp + geom_line(data=data.frame(x = c(0,1), y = c(0,1)), aes(x = x, y = y), colour = "black", linetype="dotted")
rocp <- rocp + scale_colour_manual(name="Model", labels=c("rf","orflog","orfridge","orfsvm","orfpls","rrf","grrf", "et", "wsrf")
                         , values=c("green","blue","yellow","orange","red","pink","cyan","purple","magenta"))
rocp + ggtitle("ROC curve") + xlab("False Positive Rate") + ylab("True Positive Rate")


# save model
#saveRDS(model, file = "RWC_Prediction_Model.rda")