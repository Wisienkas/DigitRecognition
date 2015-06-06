#graph_knn (Unique graphs for knn)
#- Input = dataMatrix
#- output = images
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

graph.knn.easy = function(dataFrame) {
  c <- ggplot(dataFrame, aes(x=reorder(K), y=reorder(AvgSuccess), fill=Name)) +
    geom_bar(width=0.7, stat="identity", position=position_dodge())
  c
}