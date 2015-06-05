#pre_PCA
#- INPUT = imageData
#- Output = dataFrame (PCA Data)

# 
# DEPENDENCIES 
#
if (!require("pacman")) install.packages("pacman")

# Wauw
pre.PCA <- function(imageData, coverage) {
  ## Turn them into a format readable by pca
  df <- pre_transform.transform(imageData)
  pc <- prcomp(df, scale = TRUE)
  
  cumulative <- cumsum(pc$sdev ^ 2 / sum(pc$sdev ^ 2))
  cutoff_index <- sum(cumulative < coverage) + 1
  
  pc_uncut <- predict(pc)
  
  return(pc_uncut[, 1:cutoff_index])
}