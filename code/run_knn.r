#Code needed to run KNN

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)
source('pre_KMeans.R')
source('pre_PCA.r')
source('alg_knn.r')
source('graph_knn.r')

## The easy problem
#Load the image
run_knn.easy.imageData <- img_loader.singlePerson("group6", "member2", 1)
#run_knn.easy.imageData <- img_loader.allPersons(1)

#Preprocessing
#Some error with kmeans, fix plx?
run_knn.easy.kmeans.clusters <- 100;
run_knn.easy.kmeans <- pre.KMeans(run_knn.easy.imageData, run_knn.easy.kmeans.clusters)
run_knn.easy.kmeans.tst <- pre_transform.transform(run_knn.easy.kmeans)
#Currently, lower PCA increase speed significantly with only a little decrese in accuracy, for one person
run_knn.easy.pca <- pre.PCA(run_knn.easy.imageData, 0.90)

#Run the KNN
run_knn.easy.knn <- alg.knn.easy(run_knn.easy.kmeans.tst, c(1,5,10,20,40,80), run_knn.easy.kmeans.clusters * 10)
run_knn.easy.knn <- alg.knn.easy(run_knn.easy.pca, c(1,5,10,20,40,80), 4000)
print(run_knn.easy.knn)


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
