#code needed to run random forest

if (!require("pacman")) install.packages("pacman")
pacman::p_load(randomForest)

run_randomForest.easy <- function(df, ntree) {
  folds <- pre_transform.folds(df = df, f = 3, digits_per_person = 2000)
  
  return(run_randomForest.run_folds(folds, ntree))
}

run_randomForest.run_folds <- function(folds, ntree) {
  results <- list()
  for(i in 1:length(folds)) {
    test <- list()
    train <- list()
    for(j in 1:length(folds)) {
      if(i == j) {
        test <- folds[[j]]
      } else {
        train$df <- rbind(train$df, folds[[j]]$df)
        train$cl <- factor(c(as.character(train$cl), as.character(folds[[j]]$cl)), levels = 1:10)
      }
    }
    rf <- randomForest(x = train$df, y = train$cl, ntree = ntree, xtest = test$df, ytest = test$cl)
    results[[i]] <- list()
    for(level in levels(test$cl)) {
      results[[i]][[level]] <- sum(rf$test$predicted == test$cl & test$cl == level) / sum(test$cl == level)
    }
  }
  return(results)
}

run_randomForest.hard <- function(df, ntree) {
  folds <- pre_transform.foldsPeople(df = df)
  return(run_randomForest.run_folds(folds, ntree))
}