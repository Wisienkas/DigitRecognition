#graph_knn (Unique graphs for knn)
#- Input = dataMatrix
#- output = images
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

graph.knn.easy = function(dataFrame) {
  c <- ggplot(dataFrame, aes(x=reorder(K), y=as.numeric(as.character(reorder((AvgSuccess)))), fill=Name)) +
    geom_bar(width=0.7, stat="identity", position=position_dodge()) +
    coord_cartesian(ylim=c(0.6, 1.0)) + scale_y_continuous(breaks=seq(from = 0.6, to = 1.0, by = 0.05)) +
    labs(title = 'Easy problem', x = 'K value', y = 'Success', fill = 'Settings')
  c
}

graph.knn.hard <- function(dataFrame) {
  c <- ggplot(dataFrame, aes(x=reorder(Person), y=as.numeric(as.character(reorder((AvgSuccess)))))) +
    geom_bar(width=0.7, stat="identity") + 
    coord_cartesian(ylim=c(0.4, 1.0)) + scale_y_continuous(breaks=seq(from = 0.4, to = 1.0, by = 0.05)) +
    labs(title = 'Hard problem', x = 'Group and member', y = 'Success') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  c
}