# Load libraries needed

library(jsonlite)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

# Need to set a high scientific notation penalty, or the paste function will make the sql query with scientific numbers
options("scipen"=10)

# Number of matchIDs to extract
numMatches <- 30000

# Create SQL query text
sqlQuery <- paste("select  * from public_matches where start_time > 1486583036 AND duration > 900 AND avg_mmr > 2500 AND num_mmr > 6 limit ",
                  numMatches, ";", sep = "")

# Execute query on opendota API
gameList <- fromJSON(paste("https://api.opendota.com/api/explorer?sql=", URLencode(sqlQuery), sep=""))
gameListDF <- gameList$rows

gameListDF <- arrange(gameListDF, match_id)

# Initialize the dataframe
finalList <- NULL

# Iterate through all the games we have
for (i in 1:nrow(gameListDF)) {
  
  print(paste("Parsing game: ", i))
  matchID <- gameListDF[i, "match_id"]
  
  startTime <- proc.time()[3]
  
  # Read the JSON (using error handling)
  readJSON <- tryCatch(
    fromJSON(paste("https://api.opendota.com/api/matches/", matchID, sep = "")),
    error = function (e) {"error"}
  )
  
  # Check if we got HTTP error, if so, go to next matchID
  if (readJSON == "error") {
    print("      Error in API fetching")
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
  }
  
  # If no cosmetics are shown, we assume it's not parsed and go to the next one
  if (is.null(readJSON$cosmetics)) {
    print(paste("      replay not parsed."))
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
  }
  
  # Obtain list of the position for the first 10 minute for every player
  if (is.data.frame(readJSON$players$lane_pos)) {
    lanePosList <- flatten(readJSON$players$lane_pos)
  } else {
    print(paste("      replay not parsed."))
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
  }
  
  # Game time in minutes
  gameTime <- round((readJSON$duration/60) / 10, digits = 0) * 10
  
  # Iterate through each player, getting the values we need.
  for(j in 1:10) {

    # Create a list of lane position first 10 minutes of game
    heroLanePos <- flatten(lanePosList[j,])
    lanePosLong <- as.data.frame(gather(heroLanePos))
    if(length(lanePosLong) > 0) {
      lanePosLong <- subset(lanePosLong, !is.na(lanePosLong$value)) } else {
        next
      }
    
    # We get output in weird format, so i have to do some string work to get the x and y coordintes
    lanePosLong$x <- as.numeric(substr(lanePosLong$key, 1, regexpr('\\.', lanePosLong$key)-1))
    lanePosLong$y <- as.numeric(substr(lanePosLong$key, regexpr('\\.', lanePosLong$key)+1, nchar(lanePosLong$key)))
    
    lanePosFinal <- lanePosLong[,c(3,4,2)]
    lanePosFinal$matchID <- matchID
    lanePosFinal$heroID <- readJSON$players$hero_id[j]
    lanePosFinal$playerSlot <- readJSON$players$player_slot[j]
    lanePosFinal$lane <- readJSON$players$lane[j]
    
    # long to wide format
    finalWide <- dcast(lanePosFinal,
                       matchID + heroID + playerSlot + lane ~ x + y,
                       value.var = "value")
    
    # Output the player to finalList
    if (is.null(finalList)) {
      finalList <- finalWide
    } else {
      finalList <- rbind.fill(finalList, finalWide)
    }
  }
  print(paste("      replay parsed!"))
  
  endTime <- proc.time()[3]
  totTime <- endTime - startTime
  
  if (totTime < 1) {
    Sys.sleep(1.05-totTime)
    print(proc.time()[3] - startTime)
  }
}

finalList$side <- ifelse(finalList$playerSlot < 10, "Radiant", "Dire")
finalList[is.na(finalList)] <- 0

# Save the file
saveRDS(finalList, "matches.RDS")