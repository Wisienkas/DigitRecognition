#alg_knn
#- Input = dataFrame
#- Output = dataMatrix (Data for performance, and arguments)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(class)
source('img_loader.R')
source('pre_transform.R')

alg.knn.easy = function(dataFrame, k_arr) {
  result.list <- matrix(ncol = 3, dimnames = list(c('Result'), c('K', 'Success', 'Time')), byrow = TRUE)
  result.list <- result.list[-1,]

  for(k in 1:length(k_arr)) {
    time <- 0
    success <- c()
    
    #Cross validation in 10 folds
    for(i in 1:10) {
      result.cl <- pre_transform.getClass(df = dataFrame)
      result.seq <- seq(from = i, by = 10, to = nrow(dataFrame))
      result.test <- dataFrame[result.seq,]
      result.testCL <- result.cl[result.seq]
      result.train <- dataFrame[-result.seq,]
      result.trainCL <- result.cl[-result.seq]
      
      start_time <- as.numeric(Sys.time())
      result.knn <- knn(train = result.train, test = result.test, cl = result.trainCL, k= k)
      run_time <- as.numeric(Sys.time()) - start_time
      time <- time + run_time
      
      result.table.units <- table(result.knn == result.testCL, result.testCL)[seq(from = 2, to = 20, by = 2)]
      result.table.prob <- result.table.units / (nrow(result.test) / nlevels(result.testCL))
      
      success <- c(success, result.table.prob)
    }
    
    correctness <- c()
    for(digit in 1:10) {
      index.digit <- seq(from = digit, to = length(success), by = 10);
      avg <- mean(success[index.digit])
      correctness <- c(correctness, avg)
    }
    avgSuccess <- mean(correctness)
    res <- list(k, avgSuccess, time);
    result.list <- rbind(result.list, res)
  }
  
  return (result.list)
}

alg.knn.hard <- function(dataFrame, kValue) {
  #Perform one person test vs all other person
}