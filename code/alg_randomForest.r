#alg_randomForest
#- Input = dataFrame
#- Output = dataMatrix (Data for performance, and arguments)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(randomForest)

alg_randomForest.easy <- function(df, ntree, digits_per_person = 4000) {
  folds <- pre_transform.folds(df = df, f = 10, digits_per_person = digits_per_person)
  
  return(alg_randomForest.run_folds(folds, ntree))
}

alg_randomForest.run_folds <- function(folds, ntree) {
  # time, ntree, avg correctness, digit correctness
  results <- matrix(nrow = 0, ncol = 13)
  colnames(results) <- c("ntree", "time", "avg correctness", 0:9)
  for(trees in ntree) {
    digitSuccess <- c()
    start_time <- Sys.time()
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
      rf <- randomForest(x = train$df, y = train$cl, ntree = trees, xtest = test$df, ytest = test$cl)
      pred <- c()
      for(level in levels(test$cl)) {
        pred[level] <- sum(rf$test$predicted == test$cl & test$cl == level) / sum(test$cl == level)
      }
      if(i == 1) {
        digitSuccess <- pred
      } else {
        digitSuccess <- digitSuccess + pred
      }
    }
    end_time <- Sys.time() - start_time
    digitSuccess <- digitSuccess / length(folds)
    avgCorrect <- mean(digitSuccess)
    results <- rbind(results, c("trees" = trees, "time" = end_time, "avg correctness" = avgCorrect, digitSuccess))
  }
  return(results)
}

alg_randomForest.hard <- function(df, ntree, people) {
  folds <- pre_transform.foldsPeople(df = df, people = people)
  return(alg_randomForest.run_folds(folds, ntree))
}