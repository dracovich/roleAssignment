library(Rtsne)
library(dplyr)
library(ggplot2)
library(car)
library(rgl)
library(mlr)
library(caret)
library(dbscan)
library(data.table)
source("clust.R")

# Fetch data
datamart <- readRDS("matches.RDS")
datamart <- subset(datamart, datamart$side == "Dire")

# Fetch manual data
manualLanes <- read.csv("manualLanes.csv", sep = ",")
colnames(manualLanes) <- c("matchID", 
                           "heroID",
                           "side",
                           "heroName",
                           "Lane",
                           "Role")

logicalList <- manualLanes$matchID %in% datamart$matchID
manualLanes <- manualLanes[logicalList, ]

manualGames <- datamart[datamart$matchID %in% manualLanes$matchID, ]

# Only look at one side, and limit to 15000 lines
datamartIDs <- sample(unique(datamart$matchID), 5000)

datamart <- datamart[datamart$matchID %in% datamartIDs, ]
manualGamesClean <- manualGames[!(manualGames$matchID %in% datamart$matchID), ]

datamart <- rbind(datamart, manualGamesClean)

datamart$newLane <- ifelse(datamart$is_roaming == TRUE, 4, datamart$lane)

# Remove non-relevant variables
usedData <- as.data.frame(select(datamart,
                   -is_roaming,
                   -matchID,
                   -heroID,
                   -playerSlot,
                   -xp,
                   -gold,
                   -lastHits,
                   -deny))

# Some coordinates have too few datapoints to be able to be used.
nzv <- nearZeroVar(usedData, saveMetrics = TRUE)

zeroVar <- nzv$zeroVar

datamartNew <- usedData[, !zeroVar]

tsneData <- dplyr::select(datamartNew, -lane, -newLane, -meanX, -meanY, -avgDistMed)

dupRows <- duplicated(tsneData)
tsneData <- tsneData[!dupRows, ]
datamartnewClean <- datamartNew[!dupRows, ]
datamartnewClean[is.na(datamartnewClean)] <- 0
tsneData[is.na(tsneData)] <- 0

tsne <- Rtsne(tsneData, dims = 3, perplexity=60, verbose=TRUE, max_iter = 600)
  
tsneOut <- as.data.frame(tsne$Y)
#colnames(tsneOut) <- c("X","Y")
colnames(tsneOut) <- c("X","Y","Z")

tsneOut$label <- datamartnewClean$newLane

# Add a name to each role
laneList <- c(1, 2, 3, 4, 5)
laneRoles <- c("Off", 
               "Mid", 
               "Safe",
               "Roamer",
               "Jungle")

laneDF <- data.frame(laneNum = laneList,
                     laneRole = laneRoles)

withRoles <- left_join(tsneOut, laneDF, by = c("label" = "laneNum"))

#rm(datamartNew, nzv, tsneData, usedData)

# Do density clustering
source("clust.R")
clusterVec <-  createClusters(select(withRoles, X,Y,Z), 
                              dataPointNum = 10000, 
                              clusterNum = 5,
                              method = "hierarchical")
withRoles$clust <- clusterVec


# Uncomment if you want to make gif
#movie3d( spin3d(rpm = 6), duration=10, fps = 10, dir="gif/", clean=FALSE, type = "gif")

# Download manually labeled data and see which method is most correct.

clust_sum <-  withRoles %>%
              group_by(laneRole, clust) %>%
              summarise(tot = n()) %>%
              ungroup() %>%
              arrange(-tot)

top_clust <-  clust_sum %>%
              group_by(laneRole) %>%
              arrange(-tot) %>%
              filter(row_number() == 1) %>%
              ungroup() %>%
              arrange(clust)

clustConversion <- data.frame(clusterNum = numeric(),
                              laneRoleNum = numeric())

for (i in top_clust$laneRole) {
  laneRoleNum <- which(grepl(i, laneDF$laneRole))
  clustNum <- top_clust[which(grepl(i, top_clust$laneRole)), 2]
  
  dfTemp <- data.frame(clusterNum = clustNum,
                       laneRoleNum = laneRoleNum)
  clustConversion <- rbind(clustConversion, dfTemp)
}

withRoles <- left_join(withRoles, 
                        clustConversion, 
                        by = "clust")

clusterData <- withRoles
clusterData$X <- clusterData$X + 30
clusterData$Y <- clusterData$Y + 30
clusterData$Z <- clusterData$Z + 30

combinedData <- rbind(withRoles, clusterData)

# Visualize in 3D
width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(combinedData, X,Y,Z), type="p", radius=0.1, axes=F, col = as.factor(combinedData$label),
        expand = 0, xlab = "X", ylab = "Y", zlab = "Z")
legend3d("topright", legend = laneRoles, pch = 16, col = as.factor(laneList), cex=1, inset=c(0.02))

# Cleanup of manual games before we aggregate and predict
manualLanes <- read.csv("manualLanes.csv", sep = ",")
colnames(manualLanes) <- c("matchID", 
                           "heroID",
                           "side",
                           "heroName",
                           "Lane",
                           "Role")
manualLanes <- manualLanes[manualLanes$Role != '', ]

manualLanes <- subset(manualLanes, manualLanes$side == "Dire")
manualLanes$Lane <- as.character(manualLanes$Lane)
manualLanes$Role <- as.character(manualLanes$Role)
#uniqueLanes <- count(manualLanes, Lane)
#uniqueRole <- count(manualLanes, Role)

manualLanes <- left_join(manualLanes, laneDF, by = c("Lane" = "laneRole"))
manualLanes$trueLane <- manualLanes$laneNum
  
manualLanePred <- select(datamart, matchID, playerSlot, heroID, lane, newLane)
manualLanePred <- manualLanePred[!dupRows, ]
manualLanePred$clust <- clusterData$label
manualLanePred$X <- clusterData$X
manualLanePred$Y <- clusterData$Y
manualLanePred$Z <- clusterData$Z

manualLanesFinal <- as.data.table(left_join(manualLanes, manualLanePred,
                              by = c("matchID", "heroID")))


correctPct <- function(data, testCol, trueCol) {
  library(dplyr)
  logicalList <- data[, get(testCol)] == data[, get(trueCol)]
  data$correct <- logicalList
  
  sumData <- data %>%
              group_by(Lane) %>%
              summarise(correctPct = sum(correct == TRUE) / n(),
                        incorrectPct = sum(correct == FALSE) / n())
  
  return(sumData)
}

oldOD <- correctPct(manualLanesFinal, "lane", "trueLane")
newOD <- correctPct(manualLanesFinal, "newLane", "trueLane")
clustNew <- correctPct(manualLanesFinal, "clust", "trueLane")

library(mgl)
learning_datamart <- select(manualLanesFinal, 
                            X, Y, Z,
                            trueLane)
learning_datamart$X2 <- learning_datamart$X ** 2
learning_datamart$Y2 <- learning_datamart$Y ** 2
learning_datamart$Z2 <- learning_datamart$Z ** 2
learning_datamart$XY <- learning_datamart$X * learning_datamart$Y
learning_datamart$XZ <- learning_datamart$X * learning_datamart$Z
learning_datamart$YZ <- learning_datamart$Y * learning_datamart$Z

lm_learning <- lm(as.factor(trueLane ~ ., learning_datamart)



all_data <- withRoles
all_data$label <- all_data$laneRoleNum
all_data$X <- all_data$X + 30
all_data$Y <- all_data$Y + 30
all_data$Z <- all_data$Z + 30

combinedData <- rbind(withRoles, all_data)



width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(manualLanesFinal, X,Y,Z), type="p", radius=0.1, axes=F, col = as.factor(manualLanesFinal$trueLane),
       expand = 0, xlab = "X", ylab = "Y", zlab = "Z")
legend3d("topright", legend = laneRoles, pch = 16, col = as.factor(laneList), cex=1, inset=c(0.02))
