#img_loader
#- Load half person (Used for testing)
#- Load single person
#- Load all person (Call load single person multiple times)
#OUTPUT = imageData

if (!require("pacman")) install.packages("pacman")
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#pacman::p_load(png, class, gmodels, EBImage)
pacman::p_load(png, class, gmodels, EBImage)
imageBasePath = "../digits"

img_loader.halfPerson <- function(group, member, sigmaBlur = NULL) {
  person <- img_loader.singlePerson(group, member, sigmaBlur)
  for(i in 1:length(person)) {
    person[[i]] = person[[i]][seq(from = 1, to = 400, by = 2),]
  }
  return(person)
}

img_loader.singlePerson <- function(group, member, sigmaBLur = NULL) { 
    #load the scaned images
    imageInfo <- img_loader.fileLocator(imageBasePath, group, member)
    
    #Read the files
    ciffers <- lapply(X = imageInfo$img_files, FUN = readPNG)
    
    #load the corner values
    corners <- read.csv(imageInfo$corner_file)
    corners <- trunc((corners*100)/300)
    
    #define lists to be used
    smoothed <- list(1:5)
    prepared <- list(1:5)
    gray <- list(1:5)
    
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
      
      #Gaussian smoothing
      if(is.null(sigmaBLur) || sigmaBLur == 0) {
        smoothed[[i]] = gray[[i]]
      } else{
        smoothed[[i]] <- gblur(x = gray[[i]], sigma = sigmaBLur) 
      }
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
    
    #img <- ciffers[[1]]
    #img[,,1] <- prepared[[1]]
    #img[,,2] <- prepared[[1]]
    #img[,,3] <- prepared[[1]]
    #display(img)
    
    # will add 1.0's to datasets digits with less than 380 dimenstions
    data1 <- trainingDigit
    for(i in 1:length(data1)) {
      while(ncol(data1[[i]]) != 380) {
        data1[[i]] <- cbind(data1[[i]], rep(1.0, 400))
      }
    }
    return(data1)
  }

img_loader.allPersons <- function(sigmaBLur = NULL) {
  group_folders <- list.dirs(path = imageBasePath)    
  members <- c()
  
  #Run through all the folders
  lapply(X = group_folders, FUN = function(dirPath){
    
    #The folder must contain member before we want to look in it
    if(length(grep("member", dirPath)) <  1) {
      return()
    }
    
    #Get group and member names
    group_name <- substr(dirPath, nchar(dirPath) - 13, nchar(dirPath) - 13 + 5);
    member_name <- substr(dirPath, nchar(dirPath) - 6, nchar(dirPath));
    
    #Get images for that user
    member <- img_loader.singlePerson(group_name, member_name, sigmaBLur);
    
    if(length(member) > 0 && !is.null(member)) {
      #Add row to matrix
      members <<- c(members, member);
    }
  });  
  
  return (members);
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
