pollutantmean <- function(directory=getwd(), pollutant, id = 1:332) {
  
  tryCatch(
    {
      # Create en empty numeric vector
      pMean <- numeric(0)
      
      for(mid in id){
        
        # Get data frame by the given monitor ID
        mDF <- getDF(mid, directory)
        
        # Append the pollutant values without NAs
        pMean <- c(pMean, na.omit(mDF[,pollutant]))
      }
      # Calulate mean
      mean(pMean)
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


isValidPollutant <- function(pollutant){
  
  if(pollutant != "sulfate" & pollutant != "nitrate")
    stop(call. = FALSE, "Please enter a valid pollutant. Availables ones are 'sulfate' or 'nitrate'.")
  
}


# pollutantmean <- function(directory=getwd(), pollutant, id = 1:332) {
# 
#   tryCatch(
# 
#     {
#       dirPath <- paste(getwd(), "/", directory, sep="")
# 
#       isValidDirectory(dirPath)
#       isValidPollutant(pollutant)
# 
#       pMean <- vector(mode='numeric')
# 
# 
#       for(i in 1:length(id)){
# 
#         filePath <- paste(dirPath, "/", formatC(id[i], width=3, flag=0), ".csv", sep="")
#         newf <- read.csv(file = filePath, header = TRUE, sep = ",", dec=".");
# 
#         pMean <- c(pMean, newf[,pollutant])
#       }
# 
#       mean(pMean, na.rm = TRUE);
#     },
#     error = function(e)
#     {
#       print(e)
#     }
# 
#   )
# 
# }

