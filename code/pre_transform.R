# pre_transform
# - Input = imageData
# - output = dataFrame

# Takes ImageData and transform it to DataFrame format
pre_transform.transform <- function(imageData) {

  ## Turn them into a format readable by pca
  df <- data.frame()
  for(i in imageData) {
    df <- rbind(df, i)
  }
  
  return(df)

}

pre_transform.getClass <- function(df, digits_per_person = 4000) {
  # Make classes for everyone
  classF <- c()
  for(person in 1:(nrow(df) / digits_per_person)) {
    for(i in 1:10) classF <- c(classF, rep.int(x = i, times = digits_per_person / 10))
  }
  classF <- factor(classF)
  
  return(classF)
}

# Return folds in a list of 10 df's with class association
pre_transform.folds <- function(df, f = 10) {
  classF <- pre_transform.getClass(df)
  
  # Split data into list
  folds <- list()
  for(i in 1:f) {
    fold <- seq(from = i, by = f, to = nrow(df))
    folds[[i]] <- list()
    folds[[i]]$df <- df[fold, ]
    folds[[i]]$cl <- classF[fold]
  }
  
  return(folds)
}

pre_transform.foldsPeople <- function(df, digits_per_person = 4000, people = 18) {
  classF <- pre_transform.getClass(df, digits_per_person)
  
  # Split data into list
  folds <- list()
  for(i in 1:people) {
    fold <- ((i - 1) * 4000) + 1:(i * 4000)
    folds[[i]] <- list()
    folds[[i]]$df <- df[fold, ]
    folds[[i]]$cl <- classF[fold]
  }
  
  return(folds)
}