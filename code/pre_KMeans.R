#pre_KMeans
#- Input = imageData
#- Output = dataFrame (K-Means Data)

pre.KMeans = function(imageData, classF, clusters) {
  km.result <- kmeans(x = imageData, centers = clusters, iter.max = 1000, nstart = clusters * 20)
  km <- list()
  
  km$clusters <- km.result$cluster
  km$centers <- as.data.frame(km.result$center)
  
  return(km)
}