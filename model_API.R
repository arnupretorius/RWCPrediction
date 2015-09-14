## Load Library
library(caret)
library(randomForest)

## Load Pre-trained model
model <- readRDS(file = "RWC_Prediction_Model.rda")

## Create a function to take team name inputs
## and then return a prediction for the match
predict_match <- function(homeTeam, awayTeam) {
    
    teams <- c("Namibia", "South Africa", "Argentina", "Canada", "United States",
               "Uruguay", "Japan", "England", "France", "Georgia", "Ireland",
               "Italy", "Romania", "Scotland", "Wales", "Australia",
               "Fiji", "New Zealand", "Samoa", "Tonga")
    
    teamStats <- readRDS(file="teamStats.rda")
    
    homeIndex <- which(teams == homeTeam)
    awayIndex <- which(teams == awayTeam) 
    
    rank <- readRDS("rank.rda")
    rankScore <- readRDS("rankScore.rda")
    rankChange <- readRDS("rankChange.rda")
    
    homeTeamStats <- c(teamStats[[homeIndex]], rank[homeIndex], rankScore[homeIndex], rankChange[homeIndex])
    awayTeamStats <- c(teamStats[[awayIndex]], rank[awayIndex], rankScore[awayIndex], rankChange[awayIndex])
    
    levelsx <- levels(factor(teams))
    levelsy <- levels(factor(c("loose","win")))
    newCase <- read.csv("newCase.csv")
    levels(newCase[,1]) <- levelsy
    levels(newCase[,2]) <- levelsx
    levels(newCase[,3]) <- levelsx
    newCase[1,2] <- homeTeam
    newCase[1,3] <- awayTeam
    newCase[1,4:27] <- homeTeamStats
    newCase[1,28:51] <- awayTeamStats
    
    
    ## Use the model for prediction
    y_probs <- predict(model, newCase, type="prob")
    
    ## Return the predicted class
    return(as.character(y_probs))
    
}