#code needed to run random forest

# Load required files
source("img_loader.r")
source("pre_PCA.r")
source("pre_transform.R")
source("pre_KMeans.R")
source("alg_randomForest.r")
source("graph_randomForest.r")


# Load all Data
data <- img_loader.allPersons()

result_data <- NULL

clusters <- c(10, 20, 40, 80);
pca <- c(0.9, 0.95, 0.99);

ntree <- c(50, 125, 250)
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
s <- smoothing[2]
data.smooth <- img_loader.allPersons(sigmaBLur = s)
df.test <- pre_transform.transform(imageData = data.smooth)
for(item in km_list) {
  start_time <- Sys.time();
  rf <- alg_randomForest.easy(df = item$km_data, test = df.test, ntree = ntree, digits_per_person = item$km_clusters * 10)
  rf_km <- cbind(rf, "mode" = paste("kmeans", item$km_clusters, sep = "_"))
  if(is.null(result_data)) {
    result_data <- rf_km
  } else {
    result_data <- rbind(result_data, rf_km)
  }
}
saveRDS(result_data, file = "kmeans_resultv2.RDS")

result_data <- NULL
# Running with only PCA

s <- smoothing[1]
data.smooth <- img_loader.allPersons(sigmaBLur = s)
for(p in pca) {
  start_time <- Sys.time();
  data.pca <- pre.PCA(imageData = data.smooth, coverage = p)
  rf <- alg_randomForest.easy(df = data.pca, ntree = ntree, digits_per_person = 4000)
  rf_km <- cbind(rf, "mode" = paste("pca", p, "blur", s, sep = "_"))
  if(is.null(result_data)) {
    result_data <- rf_km
  } else {
    result_data <- rbind(result_data, rf_km)
  }
}
saveRDS(result_data, file = "s0_5_PCA_result.RDS")

result_data <- NULL
s <- smoothing[2]
data.smooth <- img_loader.allPersons(sigmaBLur = s)
for(p in pca) {
  start_time <- Sys.time();
  data.pca <- pre.PCA(imageData = data.smooth, coverage = p)
  rf <- alg_randomForest.easy(df = data.pca, ntree = ntree, digits_per_person = 4000)
  rf_km <- cbind(rf, "mode" = paste("pca", p, "blur", s, sep = "_"))
  if(is.null(result_data)) {
    result_data <- rf_km
  } else {
    result_data <- rbind(result_data, rf_km)
  }
}
saveRDS(result_data, file = "s1_PCA_result.RDS")

result_data <- NULL
s <- smoothing[3]
data.smooth <- img_loader.allPersons(sigmaBLur = s)
for(p in pca) {
  start_time <- Sys.time();
  data.pca <- pre.PCA(imageData = data.smooth, coverage = p)
  rf <- alg_randomForest.easy(df = data.pca, ntree = ntree, digits_per_person = 4000)
  rf_km <- cbind(rf, "mode" = paste("pca", p, "blur", s, sep = "_"))
  if(is.null(result_data)) {
    result_data <- rf_km
  } else {
    result_data <- rbind(result_data, rf_km)
  }
}
saveRDS(result_data, file = "s2_PCA_result.RDS")

## THE HARD PROBLEM
smoothData <- img_loader.allPersons(sigmaBLur = 1)
kmeaned <- pre.KMeans(imageData = smoothData, clusters_per_digit = 20)
kmeaned.df <- pre_transform.transform(kmeaned)
normal.df <- pre_transform.transform(smoothData)
pcaData <- pre.PCA(imageData = smoothData, coverage = 0.95)
tree_number <- 250

result_hard_kmeans <- alg_randomForest.hard(df = kmeaned.df, testdf = normal.df, ntree = tree_number, people = length(smoothData) / 10, train_digit_per_person = 200)
saveRDS(result_hard_kmeans, file = "result_hard_kmeans.RDS")
result_hard_pca <- alg_randomForest.hard(df = pcaData, testdf = pcaData, ntree = tree_number, people = length(smoothData) / 10)
saveRDS(result_hard_pca, file = "result_hard_pca.RDS")


## PLOTTING HARD

result_hard_kmeans <- readRDS("result_hard_kmeans.RDS")
result_hard_pca <- readRDS("result_hard_pca.RDS")

result_hard_kmeans <- cbind(result_hard_kmeans, mode = "kmeans")
result_hard_pca <- cbind(result_hard_pca, mode = "pca")

result_hard <- rbind(result_hard_kmeans, result_hard_pca)

# rename "avg correctness" to "avgcorrectness"
colnames(result_hard)[4] <- "avgcorrectness"

gp <- readRDS("groupvector.RDS")

rows_hard <- nrow(result_hard)
length_gp <- length(gp)
fac <- rows_hard / length_gp
gp <- rep(gp, fac)
names(gp) <- 1:rows_hard

result_hard <- cbind(result_hard, GP = gp)
result_hard <- as.data.frame(result_hard)
p <- graph.randomForest.hard(result_hard)
p


## PLOTTING EASY

result_easy_kmeans <- readRDS("kmeans_resultv2.RDS")

result_easy_s05 <- readRDS("s0_5v2_PCA_result.RDS")

result_easy_s1 <- readRDS("s1v2_PCA_result.RDS")

result_easy_s2 <- readRDS("s2v2_PCA_result.RDS")

result_easy <- rbind(result_easy_kmeans, result_easy_s1, result_easy_s2)
result_easy <- as.data.frame(result_easy)

colnames(result_easy)[3] <- "avgcorrectness"

p <- graph.randomForest.easy(dataMatrix = result_easy)
p
