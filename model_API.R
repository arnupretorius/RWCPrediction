## Load Library
library(caret)
library(randomForest)
library(lubridate)

## Load Pre-trained model
model <- readRDS(file = "RWC_Prediction_Model.rda")

## Load predictors
teamStats <- readRDS(file="teamStats.rda")
rank <- readRDS("rank.rda")
rankScore <- readRDS("rankScore.rda")
rankChange <- readRDS("rankChange.rda")
teams <- readRDS("teamNames.rda")

## Create a function to take team name inputs
## and then return a prediction for the match
predict_match <- function(homeTeam, awayTeam) {
    
    homeIndex <- which(teams == homeTeam)
    awayIndex <- which(teams == awayTeam) 
    
    swap <- awayTeam == "England"
    
    if(swap){
        
        temp <- homeTeam
        homeTeam <- awayTeam
        awayTeam <- temp
        
        temp <- homeIndex
        homeIndex <- awayIndex
        awayIndex <- temp
    
    }

    
    homeTeamStats <- c(teamStats[[homeIndex]], rank[homeIndex], rankScore[homeIndex], rankChange[homeIndex])
    awayTeamStats <- c(teamStats[[awayIndex]], rank[awayIndex], rankScore[awayIndex], rankChange[awayIndex])
    
    date <- Sys.Date()
    levelsx <- levels(factor(teams))
    levelsy <- levels(factor(c("loose","win")))
    newCase <- readRDS("newCase.rda")
    newCase[1,2] <- homeTeam
    newCase[1,3] <- awayTeam
    newCase[1,4] <- factor(month(date))
    newCase[1,5] <- factor(year(date))
    newCase[1,6:29] <- homeTeamStats
    newCase[1,30:53] <- awayTeamStats
    
    
    ## Use the model for prediction
    
    
    ## Return the predicted class
    if(swap){
        y_probs <- predict(model, newCase, type="prob")
        return(as.character(rev(y_probs)))
    } else if(homeTeam == "England") {
        y_probs <- predict(model, newCase, type="prob")
        return(as.character(y_probs))
    } else {
        y_probs1 <- predict(model, newCase, type="prob")
        
        temp <- homeIndex
        homeIndex <- awayIndex
        awayIndex <- temp
        
        homeTeamStats <- c(teamStats[[homeIndex]], rank[homeIndex], rankScore[homeIndex], rankChange[homeIndex])
        awayTeamStats <- c(teamStats[[awayIndex]], rank[awayIndex], rankScore[awayIndex], rankChange[awayIndex])
        
        date <- Sys.Date()
        levelsx <- levels(factor(teams))
        levelsy <- levels(factor(c("loose","win")))
        newCase <- readRDS("newCase.rda")
        newCase[1,2] <- homeTeam
        newCase[1,3] <- awayTeam
        newCase[1,4] <- factor(month(date))
        newCase[1,5] <- factor(year(date))
        newCase[1,6:29] <- homeTeamStats
        newCase[1,30:53] <- awayTeamStats
        
        y_probs2 <- predict(model, newCase, type="prob")
        looseProb <- round((y_probs1[1] + y_probs2[2])/2,4)
        winProb <- round((y_probs1[2] + y_probs2[1])/2,4)
        return(as.character(c(looseProb, winProb)))
    }
}