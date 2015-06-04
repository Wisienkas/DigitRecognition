img_loader
- Load half person (Used for testing)
- Load single person
- Load all person (Call load single person multiple times)
OUTPUT = imageData

pre_gausianSmoothing
- Input = imageData
- Ouput = smoothedImageData (Same format as imageData)

pre_PCA
- INPUT = imageData
- Output = dataFrame (PCA Data)

pre_KMeans
- Input = imageData
- Output = dataFrame (K-Means Data)

alg_knn
- Input = dataFrame
- Output = dataMatrix (Data for performance, and arguments)

alg_randomForest
- Input = dataFrame
- Output = dataMatrix (Data for performance, and arguments)

graph_common (When needs to show same image for both algorithms)
- input = dataMatrix
- output = images

graph_randomForest (Unique graphs for random forest)
- Input = dataMatrix
- output = images

graph_knn (Unique graphs for knn)
- Input = dataMatrix
- output = images