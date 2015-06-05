img_loader
- Load half person (Used for testing)
- Load single person
- Load all person (Call load single person multiple times)
OUTPUT = imageData

pre_PCA
- INPUT = imageData
- Output = dataFrame (PCA Data)

pre_KMeans
- Input = imageData
- Output = imageData

pre_transform
- Input = imageData
- output = dataFrame

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