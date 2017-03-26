library(Rtsne)
library(dplyr)
library(ggplot2)
library(car)
library(rgl)
library(caret)

# Fetch data
datamart <- readRDS("matches.RDS")

# Only look at one side
datamart <- subset(datamart, datamart$side == "Dire")
dataSummarized <- datamart %>%
                  group_by(side, matchID) %>%
                  summarise(teamGold = sum(gold),
                            teamXP = sum(xp),
                            teamLH = sum(lastHits),
                            teamDN = sum(deny)) %>%
                  ungroup()

datamart <- left_join(datamart, dataSummarized, by = c("matchID", "side"))

datamart$xpPct <- datamart$xp / datamart$teamXP
datamart$goldPct <- datamart$gold / datamart$teamGold
datamart$LHPct <- datamart$lastHits / datamart$teamLH
datamart$DNPct <- datamart$deny / datamart$teamDN

# Remove non-relevant variables
usedData <- select(datamart,
                   -matchID,
                   -heroID,
                   -playerSlot,
                   -xp,
                   -gold,
                   -teamGold,
                   -teamXP,
                   -teamLH,
                   -teamDN,
                   -lastHits,
                   -deny)



# Some coordinates have too few datapoints to be able to be used.
nzv <- nearZeroVar(usedData, saveMetrics = TRUE)

zeroVar <- nzv$zeroVar

datamartNew <- usedData[, !zeroVar]

tsneData <- select(datamartNew, -lane, -clust)

tsne <- Rtsne(tsneData, dims = 3, perplexity=60, verbose=TRUE, max_iter = 600)
  
tsneOut <- as.data.frame(tsne$Y)
#colnames(tsneOut) <- c("X","Y")
colnames(tsneOut) <- c("X","Y","Z")

tsneOut$label <- datamartNew$lane

# Add a name to each role
laneList <- c(1,2,3,4,5)
laneRoles <- c("Off", "Mid", "Safe","Jungle(roam?)" ,"Jungle")
laneDF <- data.frame(laneNum = laneList,
                     laneRole = laneRoles)

withRoles <- left_join(tsneOut, laneDF, by = c("label" = "laneNum"))

# Do density clustering
testClust <- hclust(dist(withRoles[, 1:3]))
plot(testClust)
clusterCut <- cutree(testClust, 6)
  
withRoles$clust <- clusterCut

datamart$clust <- clusterCut
# Visualize in 3D
width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(withRoles, X,Y,Z), type="p", radius=0.1, axes=F, col = as.factor(withRoles$label),
        expand = 0, xlab = "X", ylab = "Y", zlab = "Z")

# Uncomment if you want to make gif
# movie3d( spin3d(rpm = 6), duration=10, fps = 10, dir="gif/", clean=FALSE, type = "gif")
bounty <- select(subset(datamart, datamart$heroID == 62 & clust != 6),
                 matchID, clust)
