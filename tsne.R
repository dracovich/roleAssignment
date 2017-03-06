library(Rtsne)
library(dplyr)
library(ggplot2)
library(car)
library(rgl)
library(caret)

datamart <- readRDS("matches.RDS")

datamart <- subset(datamart, datamart$side == "Dire")

usedData <- select(datamart,
                   -matchID,
                   -heroID,
                   -playerSlot)

nzv <- nearZeroVar(usedData, saveMetrics = TRUE)

zeroVar <- nzv$zeroVar

datamartNew <- sample_n(usedData[, !zeroVar], 5000)

perplIter <- c(5,10,20,30,40,50,60,70,80,90,100)


  cat(paste("Running t-SNE with perplexity", i, "\n"))
  tsne <- Rtsne(select(datamartNew, -lane), dims = 3, perplexity=60, verbose=TRUE, max_iter = 1000)
  
  tsneOut <- as.data.frame(tsne$Y)
  #colnames(tsneOut) <- c("X","Y")
  colnames(tsneOut) <- c("X","Y","Z")
  
  tsneOut$label <- datamartNew$lane
  
  laneList <- c(1,2,3,4,5)
  laneRoles <- c("Off", "Mid", "Safe","Jungle(roam?)","Jungle")
  laneDF <- data.frame(laneNum = laneList,
                       laneRole = laneRoles)
  
  withRoles <- left_join(tsneOut, laneDF, by = c("label" = "laneNum"))
  
  #ggplot(withRoles, aes(X,Y)) + geom_point(aes(color = laneRole), alpha = 1) + ggtitle(paste("t-SNE with perplexity", i))

  
  
  testClust <- hclust(dist(withRoles[, 1:3]))
  plot(testClust)
  clusterCut <- cutree(testClust, 5)
  
  withRoles$clust <- clusterCut
  
open3d()

plot3d(select(withRoles, X,Y,Z), type="p", radius=0.1, axes=F, col = as.factor(withRoles$clust),
        expand = 0, xlab = "Mean X position", ylab = "Mean Y position", zlab = "Average distac")


ggplot(tsneOut, aes(X,Y)) + geom_point(aes(color = as.factor(label))) + ggtitle(paste("t-SNE with perplexity", i))
