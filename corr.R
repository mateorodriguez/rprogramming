corr <- function(directory, threshold = 0){
  
  # Create an empty numerivc vector
  nonaNum <- numeric(0)
  
  # Get data frame of complete cases
  nonaDF <- complete("specdata")
  
  # Filter data frame by the threshold
  nonaDF <- nonaDF[nonaDF$nobs > threshold,]
  
  for(mid in nonaDF$id){
    
    # Get the data frame of the given monitor ID
    mDF <- na.omit(getDF(mid, directory))
    
    mDF_cor <- cor(mDF$sulfate, mDF$nitrate)
    nonaNum <- c(nonaNum, mDF_cor)
  }
  
  nonaNum
}

complete <- function(directory, id = 1:332){
  
  # Create an empty numeric vector
  nonaNum <- numeric(0)
  
  for(mid in id){
    
    # Get the data frame of the given monitor ID
    mDF <- getDF(mid, directory)
    
    # Count the number of complete cases and append it to vector
    nonaNum <- c(nonaNum, nrow(na.omit(mDF)))
  }
  
  # Return the data frame
  data.frame(id = id, nobs = nonaNum)
}


getDF <- function(mid, directory){
  
  tryCatch(
    {
      # Validate file path
      isValidDirectory(paste("./", directory, sep=""))
      
      # Build the file path
      filePath <- paste("./", directory, "/", formatC(mid, width = 3, flag = 0), ".csv", sep="")
      
      # Return the data frame
      mDF <- read.csv(filePath)
    },
    error = function(e)
    {
      print(e)
    }
  )
}


isValidDirectory <- function(directory){
  
  if(!dir.exists(directory))
    stop(call. = FALSE, "Specified directory does not exist.")
  
}



# corr_last <- function(directory, threshold = 0){
#   
#   tryCatch(
#     
#     {
#       dirPath <- paste("./", directory, sep="")
#       isValidDirectory(dirPath)
#       
#       files <- list.files(path=dirPath, pattern = "*.csv", full.names = TRUE)
#       
#       csv_sulf <- data.frame(sulfate=numeric(0))
#       csv_nit <- data.frame(nitrate=numeric(0))
#       
#       corrsNum <- numeric(0)
#       
#       for(i in 1:length(files)){
#         
#         csv <- read.csv(file = files[i], header = TRUE, sep=",", dec=".")
#         if(complete(directory, i)["nobs"] > threshold){
#           
#           # csv_sulf <- rbind(csv_sulf, csv[complete.cases(csv),][c(2)])
#           # csv_nit <- rbind(csv_nit, csv[complete.cases(csv),][c(3)])
#           
#           thisCor <- cor(csv[complete.cases(csv),][c(2)], csv[complete.cases(csv),][c(3)])
#           corrsNum <- c(corrsNum, thisCor)
#         }
#         
#       }
#       # cor(csv_sulf[[c(1)]], csv_nit[[c(1)]])
#       corrsNum
#     },
#     error = function(e)
#     {
#       print(e)
#     }
#   )
#   
#   
#   
# }

