#code needed to run random forest

if (!require("pacman")) install.packages("pacman")
pacman::p_load(randomForest)

run_randomForest.easy <- function(df) {
  folds <- pre_transform.folds(df = df)
  
  for(i in 1:length(folds)) {
    test <- list()
    train <- list()
    for(j in 1:length(folds)) {
      if(i == j) {
        test <- folds[[j]]
      } else {
        train$df <- rbind(train$df, folds[[j]]$df)
        train$cl <- factor(c(as.character(train$cl), as.character(folds[[j]]$df)))
      }
    }
  }
}

run_randomForest.hard <- function(df) {
  folds <- pre_transform.foldsPeople(df = df)
  
}