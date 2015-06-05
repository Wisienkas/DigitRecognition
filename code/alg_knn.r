#alg_knn
#- Input = dataFrame
#- Output = dataMatrix (Data for performance, and arguments)

if (!require("pacman")) install.packages("pacman")
#pacman::p_load()
source('img_loader.R')

alg.knn.easy = function(dataFrame, k_arr) {
  #Perform 90/10 split, with random split
  retList <- alg.knn.randomSplit(dataFrame, 0.9)
  
  knn <- c()
  knn$testData <- retList[[2]]
  knn$testClass <- retList[[4]]
  knn$trainData <- retList[[1]]
  knn$trainClass <- retList[[3]]
  
  knn$model <- knn(train = knn$trainData, test = knn$testData, cl = knn$trainClass, k_arr)
  
  return (knn)
}

alg.knn.randomSplit <- function(dataFrame, split) {
  #randomly split data in a balanced maner:
  trainL = list(1:length(dataFrame))
  testL = list(1:length(dataFrame))
  for(Nr in 0:(length(dataFrame)-1))
  {
    amountEachNumber = nrow(dataFrame[[Nr+1]]);
    
    set.seed(1) ## make randomness reproducible here
    rand <- sample(amountEachNumber) #generate  a randomly ordered indexing list the size of the datasample
    
    vector = c(1:amountEachNumber);
    for(i in 1:trunc(amountEachNumber*split))
    {
      vector[i] = 1;
    }
    for(i in trunc(amountEachNumber*split)+1:amountEachNumber)
    {
      vector[i] = 2;
    }
    splittingIndexer = vector[rand]
    splitData <- split.data.frame(dataFrame[[Nr+1]], factor(splittingIndexer))
    
    trainL[[Nr+1]] <- splitData[[1]]
    testL[[Nr+1]]<- splitData[[2]]
  }  
  
  training <- trainL[[1]]
  testing <- testL[[1]]
  trainClass <- rep(0,nrow(trainL[[1]]) )
  testClass <- rep(0,nrow(testL[[1]]) )
  for(i in 2:10)
  {
    training <- rbind(training, trainL[[i]])
    testing <- rbind(testing, testL[[i]])
    
    trainClass <- append(trainClass, rep(i-1,nrow(trainL[[i]]) ) )
    testClass <- append(testClass, rep(i-1,nrow(testL[[i]]) ) )
  }
  trainClassF <- factor(trainClass)
  testClassF <- factor(testClass)
  
  return(list(training, testing, trainClassF, testClassF))
}

alg.knn.hard <- function(dataFrame, kValue) {
  #Perform one person test vs all other person
}