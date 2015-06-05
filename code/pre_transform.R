# pre_transform
# - Input = imageData
# - output = dataFrame

pre_transform.transform <- function(imageData) {

  ## Turn them into a format readable by pca
  df <- data.frame()
  for(i in imageData) {
    df <- rbind(df, i)
  }
  
  return(df)

}