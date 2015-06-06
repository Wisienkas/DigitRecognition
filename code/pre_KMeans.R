#pre_KMeans
#- Input = imageData
#- Output = dataFrame (K-Means Data)

pre.KMeans = function(imageData, clusters_per_digit) {
  for(i in 1:length(imageData)) {
    km <- kmeans(x = imageData[[i]], iter.max = 20, centers = clusters_per_digit, nstart = min(c(clusters_per_digit * 5, length(imageData[[i]]) / 10)))
    imageData[[i]] <- km$centers
  }
  
  return(imageData)
}