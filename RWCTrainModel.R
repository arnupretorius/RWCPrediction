### International Rugby game prediction model ###

# load libraries
library(caret)
library(randomForest)

# load data
data <- readRDS("RWCData.rda")

# tune Grid
rfGrid <-  expand.grid(mtry = c(1, 7, 10, 15, 20, 25, 30, 35, 40, 45, 50))

# Tune using 5-fold cross-validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)

# Train classifier
model <- train(x=data[,-1], y=data[,1], method="rf", ntree=1000,trControl=fitControl, tuneGrid=rfGrid)

# save model
saveRDS(model, file = "RWC_Prediction_Model.rda")