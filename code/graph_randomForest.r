#graph_randomForest (Unique graphs for random forest)
#- Input = dataMatrix
#- output = images

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

graph.randomForest.hard = function(dataMatrix){
  
  graphy <- ggplot(data = dataMatrix, aes(x = reorder(GP), y = as.numeric(as.character(reorder((avgcorrectness)))), fill = factor(reorder(mode)))) +
    geom_bar(width=0.8, stat="identity", position=position_dodge()) + 
    scale_y_continuous(breaks = seq(from = 0.35, to = 1.0, by = 0.05)) + 
    coord_cartesian(ylim=c(0.35, 1.0)) +
    labs(title = 'Hard problem', x = 'Group and member', y = 'Success', fill = "Pre-Processing") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  return (graphy)
}

graph.randomForest.easy = function(dataMatrix){
  
  graphy <- ggplot(data = dataMatrix, aes(x = reorder(mode), y = as.numeric(as.character(reorder((avgcorrectness)))), fill = factor(reorder(ntree)))) +
    geom_bar(width=0.8, stat="identity", position=position_dodge()) + 
    scale_y_continuous(breaks = seq(from = 0.35, to = 1.0, by = 0.05)) + 
    coord_cartesian(ylim=c(0.5, 1.0)) +
    labs(title = 'Easy problem', x = 'Pre-Processing', y = 'Success', fill = "Trees") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  return (graphy)
}