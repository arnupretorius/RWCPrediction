#### Construct WRC data set ####

# The Teams
teams <- c("Namibia", "South Africa", "Argentina", "Canada", "United States",
           "Uruguay", "Japan", "England", "France", "Georgia", "Ireland",
           "Italy", "Romania", "Scotland", "Wales", "Australia",
           "Fiji", "New Zealand", "Samoa", "Tonga")

# Get rank data
library(XML)
library(RCurl)
Nteams <- length(teams)
rank <- numeric(Nteams)
rankScore <- numeric(Nteams)
rankChange <- numeric(Nteams)

urlRankScore <- "https://en.wikipedia.org/wiki/World_Rugby_Rankings"
tabs1 <- getURL(urlRankScore)
tabs1 <- readHTMLTable(tabs1, stringsAsFactors = F)

urlRank <- "http://wrr.live555.com/"
tabs2 <- getURL(urlRank)
tabs2 <- readHTMLTable(tabs2, stringsAsFactors = F)
for(i in 1:length(tabs2[[2]][,2])){
    index <- which(teams == tabs2[[2]][i,2])
    if(length(index) != 0){
        rank[index] <- as.numeric(tabs2[[2]][i,1])
        if(tabs2[[2]][i,4] == ""){
            rankChange[index] <- 0
        } else {
            rankChange[index] <- as.numeric(tabs2[[2]][i,4])
        }
    }
}

for(i in 1:length(tabs1[[1]][,4])){
    index <- which(teams == tabs1[[1]][i,3])
    if(length(index) != 0){
        rankScore[index] <- as.numeric(tabs1[[1]][i,4])
    }
}

saveRDS(rank, "rank.rda")
saveRDS(rankScore, "rankScore.rda")
saveRDS(rankChange, "rankChange.rda")

teamLoop <- tolower(gsub(" ", "", teams, fixed = TRUE))

# Get match data
matchDate <- NULL
homeTeam <- NULL
scores <- NULL
awayTeam <- NULL

otherVarList <- list()

for(i in 1:Nteams){
    fileUrl <- paste("http://www.rugbydata.com/", teamLoop[i], "/gamesplayed/", sep="")
    doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
    matchDate <- c(matchDate, xpathSApply(doc, "//td[@class='match-date']", xmlValue)) 
    homeTeam <- c(homeTeam, xpathSApply(doc, "//td[@class='home-team']", xmlValue)) 
    scores <- c(scores, xpathSApply(doc, "//a[@class='match-score']", xmlValue)) 
    awayTeam <- c(awayTeam, xpathSApply(doc, "//td[@class='away-team']", xmlValue)) 
    otherVarList[[i]] <- as.numeric(xpathSApply(doc, "//span[@class='rdnumeric']", xmlValue))
}  

saveRDS(otherVarList, file = "teamStats.rda")

# Create date vector and compute linear weights
dates <- as.Date(matchDate, format="%d %B %Y")
numDates <- as.numeric(dates[order(dates)])
weights <- (numDates-min(numDates))/(max(numDates)-min(numDates))

# Extract scores
tempScores <- strsplit(scores, "-")

outcome <- NULL
ties <- NULL
countTies <- 1

# Populate win and loss labels for each matche while indexing tied matches
for(i in 1:length(tempScores)){
   diff <- as.numeric(tempScores[[i]][1]) - as.numeric(tempScores[[i]][2])
    if(diff > 0){
        outcome[i] <- "win"
    } else if (diff < 0){
        outcome[i] <- "loose"
    } else {
        ties[countTies] <- i
        countTies <- countTies + 1
    }
}

# Remove tied matches and matches not containing world cup teams
interData <- data.frame(outcome=(factor(outcome)), homeTeam, awayTeam, dates)
interData <- interData[-ties,]
countries <- c(homeTeam, awayTeam)
remove <- NULL
count <- 1
for(i in 1:nrow(interData)){
    if(!is.element(interData[i,2], teams)){
        remove[count] <- i
        count <- count+1
    } else if(!is.element(interData[i,3], teams)){
        remove[count] <- i
        count <- count+1
    }
}
interData <- interData[-remove,]
interData[,2] <- factor(interData[,2])
interData[,3] <- factor(interData[,3]) 
interData <- interData[order(interData$dates),]
interData <- interData[!duplicated(interData),]

# remove corresponding weights
weights <- weights[-ties]
weights <- weights[-remove]
weights <- weights[!duplicated(interData)]

# Add game date variables
library(lubridate)
month <- month(interData$dates)
year <- year(interData$dates)
interData <- interData[,-4]
interData$month <- month
interData$year <- year

# Only select matches as far back as the previous world cup
weights <- weights[interData$year > 2012]
interData <- interData[interData$year > 2012,]


# Add remaining variables
homeMat <- matrix(0, nrow=nrow(interData), ncol=24)
awayMat <- matrix(0, nrow=nrow(interData), ncol=24)

for(i in 1:nrow(interData)){
    homeIndex <- which(teams == interData[i,2])
    awayIndex <- which(teams == interData[i,3])
    homeMat[i,] <- weights[i]*c(otherVarList[[homeIndex]], rank[homeIndex], rankScore[homeIndex], rankChange[homeIndex])
    awayMat[i,] <- weights[i]*c(otherVarList[[awayIndex]], rank[awayIndex], rankScore[awayIndex], rankChange[awayIndex])
}

# Export data as csv file
extraData <- data.frame(cbind(homeMat, awayMat))
data <- data.frame(interData, extraData)

dataColNames <- c("Outcome", "HomeTeam", "AwayTeam",
                  "month", "year",
              "GamesPlayedHome", "GamesWonHome", "GamesLostHome",
              "GamesDrawnHome", "LongestWinningStreakHome",
              "LongestLosingStreak Home",
              "TeamsPlayedHome", "TeamsBeatenHome", 
              "TeamsBeatenByHome",
              "TeamsDrawnWithHome", "GroundsPlayedAtHome", 
              "LargestPointsForHome",
              "LargestPointsAgainstHome", 
              "LargestWinningMarginHome", 
              "LargestLosingMarginHome", "TotalPointsForHome", 
              "AvgPointsForHome",
              "TotalPointsAgainstHome", "AvgPointsAgainstHome", 
              "TotalPointsDifferenceHome", "AvgPointsDifferenceHome",
              "RankHome", "RankScoreHome", "RankChangeHome",
              "GamesPlayedAway", "GamesWon Away", "GamesLostAway",
              "GamesDrawnAway", "LongestWinning StreakAway",
              "LongestLosingStreakAway",
              "TeamsPlayedAway", "TeamsBeatenAway", 
              "TeamsBeatenByAway",
              "TeamsDrawnWithAway", "GroundsPlayedAtAway", 
              "LargestPointsForAway",
              "LargestPointsAgainstAway", 
              "LargestWinningMarginAway", 
              "LargestLosingMarginAway", "TotalPointsForAway", 
              "AvgPointsForAway",
              "TotalPointsAgainstAway", "AvgPointsAgainstAway", 
              "TotalPointsDifferenceAway", "AvgPointsDifferenceAway",
              "RankAway", "RankScoreAway", "RankChangeAway")

colnames(data) <- dataColNames
write.table(data[1,], "newCase.csv", sep=",")
write.table(data, "RWCData.csv", sep=",")