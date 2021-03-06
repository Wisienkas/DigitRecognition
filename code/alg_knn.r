#alg_knn
#- Input = dataFrame
#- Output = dataMatrix (Data for performance, and arguments)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(class)
source('img_loader.R')
source('pre_transform.R')

alg.knn.easy = function(dataFrame, k_arr, digitsPrPerson, ident) {
  result.list <- matrix(nrow = 0, ncol = 13)
  colnames(result.list) <- c('K', 'AvgSuccess','Name',0:9)

  for(k in 1:length(k_arr)) {
    success <- c()
    digits <- c()
    
    #Cross validation in 10 folds
    for(i in 1:10) {
      result.cl <- pre_transform.getClass(df = dataFrame, digits_per_person = digitsPrPerson)
      result.seq <- seq(from = i, by = 10, to = nrow(dataFrame))
      result.test <- dataFrame[result.seq,]
      result.testCL <- result.cl[result.seq]
      result.train <- dataFrame[-result.seq,]
      result.trainCL <- result.cl[-result.seq]

      result.knn <- knn(train = result.train, test = result.test, cl = result.trainCL, k= k)
      result.table.units <- table(result.knn == result.testCL, result.testCL)[seq(from = 2, to = 20, by = 2)]
      result.table.prob <- result.table.units / (nrow(result.test) / nlevels(result.testCL))
      
      success <- c(success, result.table.prob)
    }
    
    correctness <- c()
    for(digit in 1:10) {
      index.digit <- seq(from = digit, to = length(success), by = 10);
      avg <- mean(success[index.digit])
      correctness <- c(correctness, avg)
      digits <- c(digits, avg)
    }
    avgSuccess <- mean(correctness)
    res <- c(as.numeric(k_arr[[k]]), as.double(avgSuccess), ident, digits);
    result.list <- rbind(result.list, res)
  }
  return (result.list)
}

alg.knn.hard <- function(dataFrame, k_val, personCount, digitsPrPerson) {
  all.list <- matrix(nrow = 0, ncol = 12)
  colnames(all.list) <- c('Person', 'AvgSuccess', 0:9)
  
  for(person in 1:personCount) {
    index <- person - 1 
    start <- index * digitsPrPerson
    end <- (start + digitsPrPerson)
    if(index > 0)
    {
      end <- end - 1
    }
    
    result.test <- dataFrame[(start:end),]
    print(dim(result.test))
    result.testCL <- pre_transform.getClass(result.test, digitsPrPerson)
    
    result.train <- dataFrame[-(start:end),]
    print(dim(result.train))
    result.trainCL <- pre_transform.getClass(result.train, digitsPrPerson)
    
    result.knn <- knn(train = result.train, test = result.test, cl = result.trainCL, k= k_val)
    result.table.units <- table(result.knn == result.testCL, result.testCL)[seq(from = 2, to = 20, by = 2)]
    result.table.prob <- result.table.units / (nrow(result.test) / nlevels(result.testCL))
      
    success <- c(result.table.prob)
    
    correctness <- c()
    for(digit in 1:10) {
      index.digit <- seq(from = digit, to = length(success), by = 10);
      avg <- mean(success[index.digit])
      correctness <- c(correctness, avg)
      digits <- c(digits, avg)
    }
    avgSuccess <- mean(correctness)
    
    res <- c(paste('Person', person), as.double(avgSuccess), digits);
    all.list <- rbind(all.list, res)
  }
  
  return (all.list)
}