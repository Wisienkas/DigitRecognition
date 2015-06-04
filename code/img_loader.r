#img_loader
#- Load half person (Used for testing)
#- Load single person
#- Load all person (Call load single person multiple times)
#OUTPUT = imageData

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, class, EBImage, gmodels)

img_loader.halfPerson <- function(group, member) {
}

img_loader.singlePerson <- function(group, member) { 
    #load the scaned images
    imageInfo <- img_loader.fileLocator("../digits", group, member)
    ciffers <- lapply(X = imageInfo$img_files, FUN = readPNG)
    
    #load the corner values
    corners <- read.csv(imageInfo$corner_file)
    corners <- trunc((corners*100) /300)
    
    #define lists to be used
    gray <- list(1:5)
    smoothed <- list(1:5)
    prepared <- list(1:5)
    
    #convert the images to gray scale.
    for(i in 1:5) {
      r <-ciffers[[i]][,,1]
      g <-ciffers[[i]][,,2]
      b <-ciffers[[i]][,,3]
      gray[[i]] <- (r+g+b)/3
    }
    
    #smooth images
    for(i in 1:5) {
      kernel <- matrix(1,3,3)
      kernel <- kernel/9
      
      smoothed[[i]] <- filter2(gray[[i]], kernel)
    }
    
    #generate image that is prepared for learning and visualization
    for(i in 1:5) {
      prepared[[i]] <- smoothed[[i]]
    }
    
    #extract individual ciffers
    xStep <- (corners[1,7]-corners[1,1])/20;
    yStep <- (corners[1,8]-corners[1,2])/20;
    xStepT <- trunc(xStep)
    yStepT <- trunc(yStep)
    
    tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
    trainingDigit <- list(1:10);
    
    for(pages in 1:5) {
      for(box in 1:2) {
        for(cifX in 1:20) {
          aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
          for(cifY in 1:20) {
            aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
            for(px in 1:xStepT-2) {
              for(py in 1:yStepT-2) {
                tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
              }
            }
          }
        }
        trainingDigit[[(pages-1)*2 + box]] <- tempM
      }
    }
    
    return(trainingDigit)
  }

img_loader.allPersons <- function() {
}

img_loader.fileLocator <- function(basePath, group, member) {
  #Directory to image
  directory <- paste(basePath, "/", group, "/", member, sep = "");
  
  #All the files in the folder, which are PNG files
  files <- list.files(directory, pattern = ".png", full.names = TRUE);
  
  #The member object
  corner_file <- paste(directory, '/Corners.txt', sep = "");
  img_files <- NULL;
  
  #Check if the file contains the DPI we want
  lapply(X = files, FUN = function(fileName){     
    #Add files to list
    img_files <<- c(img_files, fileName)
  });
  
  #Prepare information to be added to the matrix
  file_info <- c()
  file_info$group <- group
  file_info$member <- member
  file_info$img_files <- img_files
  file_info$corner_file <- corner_file
  return (file_info);
}