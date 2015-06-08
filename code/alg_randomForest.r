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

alg_randomForest.hard <- function(df, testdf, ntree, people, train_digit_per_person = 4000, test_digit_per_person = 4000) {
  trainfolds <- pre_transform.foldsPeople(df = df, people = people, digits_per_person = train_digit_per_person)
  testfolds <- pre_transform.foldsPeople(df = testdf, people = people, digits_per_person = test_digit_per_person)
  
  return(alg_randomForest.run_folds_result(trainfolds = trainfolds, testfolds = testfolds, ntree))
}

alg_randomForest.run_folds_result <- function(trainfolds, testfolds, ntree) {
  results <- matrix(nrow = 0, ncol = 14)
  colnames(results) <- c("person", "ntree", "time", "avg correctness", 0:9)
  
  for(trees in ntree) {
    for(i in 1:length(trainfolds)) {
      start_time <- Sys.time()
      test <- list()
      train <- list()
      for(j in 1:length(trainfolds)) {
        if(i == j) {
          test <- testfolds[[j]]
        } else {
          train$df <- rbind(train$df, trainfolds[[j]]$df)
          train$cl <- factor(c(as.character(train$cl), as.character(trainfolds[[j]]$cl)), levels = 1:10)
        }
      }
      rf <- randomForest(x = train$df, y = train$cl, ntree = trees, xtest = test$df, ytest = test$cl)
      pred <- c()
      for(level in levels(test$cl)) {
        pred[level] <- sum(rf$test$predicted == test$cl & test$cl == level) / sum(test$cl == level)
      }
      end_time <- Sys.time() - start_time
      avgCorrect <- mean(pred)
      results <- rbind(results, c("person" = i, "trees" = trees, "time" = end_time, "avg correctness" = avgCorrect, pred))
    }
  }
  return(results)
}



