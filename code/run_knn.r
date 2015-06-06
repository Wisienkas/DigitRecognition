#Code needed to run KNN

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)
source('pre_KMeans.R')
source('pre_PCA.r')
source('alg_knn.r')
source('graph_knn.r')

## The easy problem
#Load the image
blur_arr <- c(0.5, 1, 2)
PCA_arr <- c(0.90, 0.95, 0.99)


k_arr <- c(1,5,10,20,50)

dataFrame <- data.frame();

for(blur in 1:length(blur_arr)){
  for(pca in 1:length(PCA_arr)) {
    tmp.data <- img_loader.singlePerson("group6", "member2", blur_arr[[blur]])
    tmp.pca <- pre.PCA(tmp.data, PCA_arr[[pca]])
    tmp.knn <- alg.knn.easy(tmp.pca, k_arr, 4000, paste('B', blur_arr[[blur]], 'PCA', PCA_arr[[pca]], sep = ''))
    dataFrame <- rbind(dataFrame, tmp.knn)
  }
}

c <- ggplot(data=as.data.frame(dataFrame), aes(x=factor(K), y=AvgSuccess, fill=Name)) +
  #coord_cartesian(ylim = c(0.9, 1)) +
  geom_bar(width=0.7, stat="identity", position=position_dodge())
c

graph.knn.easy(as.data.frame(dataFrame), 0.5, 1)
run_knn.easy.imageData <- img_loader.singlePerson("group6", "member2", 1)
#run_knn.easy.imageData <- img_loader.allPersons(1)
run_knn.easy.pca <- pre.PCA(run_knn.easy.imageData, 0.95)

#Preprocessing
#Some error with kmeans, fix plx?
run_knn.easy.kmeans.clusters <- 100;
run_knn.easy.kmeans <- pre.KMeans(run_knn.easy.imageData, run_knn.easy.kmeans.clusters)
run_knn.easy.kmeans.tst <- pre_transform.transform(run_knn.easy.kmeans)
#Currently, lower PCA increase speed significantly with only a little decrese in accuracy, for one person
run_knn.easy.pca <- pre.PCA(run_knn.easy.imageData, 0.85)

#Run the KNN
run_knn.easy.knn <- alg.knn.easy(run_knn.easy.kmeans.tst, c(1,5,10,20,40,80), run_knn.easy.kmeans.clusters * 10)
run_knn.easy.knn <- alg.knn.easy(run_knn.easy.pca, c(1,5,10,20,50), 4000, 'PCA')
print(run_knn.easy.knn)

graph.knn.easy(as.data.frame(run_knn.easy.knn), 0.5, 1)

dataFrameOne <- run_knn.easy.knn
dataFrameTwo <- run_knn.easy.knn

dataFrameOne <- cbind(dataFrameOne, 'Name' = 'PCA')
dataFrameTwo <- cbind(dataFrameTwo, 'Name' = 'NO PCA')
dataFrameTwo

dataFrameThree <- rbind(dataFrameOne, dataFrameTwo)

#Create graph
### - TODO

## The hard problem
#Load the image
#run_knn.hard.imageData <- img_loader.singlePerson("group6", "member2", 1)
run_knn.hard.imageData <- img_loader.allPersons(1)

#Preprocessing
run_knn.hard.pca <- pre.PCA(run_knn.hard.imageData, 0.90)

#Run the knn
run_knn.hard.knn <- alg.knn.hard(run_knn.hard.pca, c(1,5,10,20,40,80), 10)
print(run_knn.hard.knn)

#Create graph
### - TODO
