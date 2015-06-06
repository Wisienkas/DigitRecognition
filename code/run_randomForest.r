#code needed to run random forest

# Load required files
source("img_loader.R")
source("pre_PCA.R")
source("pre_transform.R")
source("pre_KMeans.R")
source("alg_randomForest.R")


# Load all Data
data <- img_loader.allPersons()
data1 <- data
for(i in 1:length(data1)) {
  while(ncol(data1[[i]]) != 380) {
    data1[[i]] <- cbind(data1[[i]], rep(1.0, 400))
  }
}

clusters <- c(10, 20, 40, 80);
km_rf_result <- list()
km_pca_rf_result <- list()
km_times <- list()
km_pca_times <- list()

# Running with only kmeans
for(i in 1:length(clusters)) {
  start_time <- Sys.time();
  print("Starting")
  km_data <- pre.KMeans(imageData = data1, clusters_per_digit = clusters[i])
  
  km_data_df <- pre_transform.transform(imageData = km_data)
  print("Doing random forest")
  km_rf_result[[i]] <- alg_randomForest.easy(df = km_data_df, ntree = sqrt(nrow(km_data_df)), digits_per_person = clusters[i] * 10)
  km_times[[i]] <- Sys.time() - start_time 
}

# PCA(95 %) and Kmeans
for(i in 1:length(clusters)) {
  start_time <- Sys.time();
  km_data <- pre.KMeans(imageData = data, clusters_per_digit = clusters[i])
  
  km_pca_data_df <- pre.PCA(imageData = km_data, coverage = 0.95)
  
  km_pca_rf_result[[i]] <- alg_randomForest.easy(df = km_pca_data_df, ntree = sqrt(nrow(km_data_df)), digits_per_person = clusters[i] * 10)
  km_times[[i]] <- Sys.time() - start_time 
}


