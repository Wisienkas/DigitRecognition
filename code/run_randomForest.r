#code needed to run random forest

# Load required files
source("img_loader.r")
source("pre_PCA.r")
source("pre_transform.R")
source("pre_KMeans.R")
source("alg_randomForest.r")


# Load all Data
data <- img_loader.allPersons()

result_data <- NULL

clusters <- c(10, 20, 40, 80);
pca <- c(0.9, 0.95, 0.99);

ntree <- c(250, 750, 1500)
smoothing <- c(0.5, 1, 2)

km_list <- list()

for(i in 1:length(clusters)) {
  start_time <- Sys.time();
  cluster <- clusters[i]
  km_data <- pre.KMeans(imageData = data, clusters_per_digit = cluster)
  end_time <- Sys.time() - start_time
  km_list[[i]] <- list("km_clusters" = cluster, 
                       "km_data" = pre_transform.transform(imageData = km_data), 
                       "km_time" = end_time)
}
saveRDS(km_list, file = "km_list.RDS")

result_data <- NULL
# Running with only kmeans
for(item in km_list) {
  start_time <- Sys.time();
  rf <- alg_randomForest.easy(df = item$km_data, ntree = ntree, digits_per_person = item$km_clusters * 10)
  rf_km <- cbind(rf, "mode" = paste("kmeans", item$km_clusters, sep = "_"))
  if(is.null(result_data)) {
    result_data <- rf_km
  } else {
    result_data <- rbind(result_data, rf_km)
  }
}
saveRDS(result_data, file = "kmeans_result.RDS")

result_data <- NULL
# Running with only PCA
for(s in smoothing) {
  for(p in pca) {
    start_time <- Sys.time();
    data.pca <- pre.PCA(imageData = data, coverage = p)
    rf <- alg_randomForest.easy(df = data.pca, ntree = ntree, digits_per_person = 4000)
    rf_km <- cbind(rf, "mode" = paste("pca", p, "blur", s, sep = "_"))
    if(is.null(result_data)) {
      result_data <- rf_km
    } else {
      result_data <- rbind(result_data, rf_km)
    }
  }
}
saveRDS(result_data, file = "PCA_result.RDS")

