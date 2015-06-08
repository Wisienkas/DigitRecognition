#graph_knn (Unique graphs for knn)
#- Input = dataMatrix
#- output = images
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

graph.knn.easy = function(dataFrame) {
  c <- ggplot(dataFrame, aes(x=reorder(K), y=reorder(AvgSuccess), fill=Name)) +
    geom_bar(width=0.7, stat="identity", position=position_dodge()) +
    labs(title = 'Easy problem', x = 'K value', y = 'Success', fill = 'Settings')
  c
}

graph.knn.hard <- function(dataFrame) {
  c <- ggplot(dataFrame, aes(x=reorder(Person), y=reorder(AvgSuccess))) +
    geom_bar(width=0.7, stat="identity") + 
    labs(title = 'Hard problem', x = 'Group and member', y = 'Success')
  c
}