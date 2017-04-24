createClusters <- function(clustData, 
                           dataPointNum = 5000, 
                           clusterNum = 5,
                           method = "gaussian") {
  library(mclust)
  library(dplyr)
  library(rgl)
  
  if (method == "gaussian") {
    ################## Dire ####################################
    
    # When looking at the data in 3D, it looks a bit like gaussian distributions, so I've used the clustCombi function
    # from mclust to fit gaussian clustering to the data, this takes a good while however, but i've found smaller
    # sample sizes gave unreliable results.
    
    gaussClust <- clustCombi(sample_n(clustData, dataPointNum))
    
    # Optional printing of all the different 2D combinations
    # summary(direClust, parameters = TRUE)
    # plot(direClust$MclustOutput, what = "classification")
    
    # Make predictions based on the clustering for the entire data.
    predictions <- predict(gaussClust$MclustOutput, 
                           clustData)
    clustData$cluster <- predictions$classification
    
    # It fits 1-9 different gaussians to the data, and provides you with a map of how to convert from 9 clusters
    # down to fewer, we're interested in 5 discrete ones, for safelane, offlane, mid, jungle and roaming.
    
    # Here we create the mapping file
    
    clustNum <- data.frame(newClust = unlist(gaussClust$classification[clusterNum]), 
                           c9 = unlist(gaussClust$classification[9]))
    clustCount <- dplyr::count(clustNum, 
                               newClust,c9)
    clustMap <- subset(clustCount, 
                       clustCount$n > 10)

    # And merge it onto the direData (the select statement is just so i can run this code multiple times without
    # reloading the gameData all over)
    newClust <- left_join(clustData, 
                          clustMap, 
                          by = c("cluster" = "c9"))   
    clusterOut <- newClust$newClust
  
    
  } else if (method == "kmeans"){
    kmeansClust <- kmeans(clustData, clusterNum)
    clusterOut <- kmeansClust$cluster
  } else if (method == "hierarchical") {
    hierarchicalClust <- hclust(dist(clustData), method = "centroid")
    clusterOut <- cutree(hierarchicalClust, clusterNum)
    
    
    outputClust <- clustData
    outputClust$cluster <- clusterOut
  }
  
  return(clusterOut)
}