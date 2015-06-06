#graph_knn (Unique graphs for knn)
#- Input = dataMatrix
#- output = images
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

graph.knn.easy = function(dataFrame, yMin, yMax) {
  c <- ggplot(data=dataFrame, aes(x=factor(K), y=AvgSuccess, fill=Name)) +
    coord_cartesian(ylim = c(yMin, yMax)) +
    geom_bar(width=0.5, stat="identity", position=position_dodge())
  c
}