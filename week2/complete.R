# complete <- function(directory, id = 1:332){
#   
#   # Create an empty numeric vector
#   nonaNum <- numeric(0)
#   
#   for(mid in id){
#     
#     # Get the data frame of the given monitor ID
#     mDF <- getDF(mid, directory)
#     
#     # Count the number of complete cases and append it to vector
#     nonaNum <- c(nonaNum, nrow(na.omit(mDF)))
#   }
#   
#   # Return the data frame
#   data.frame(id = id, nobs = nonaNum)
# }


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


complete <- function(directory, id = 1:332){

  tryCatch(

    {
      dirPath <- paste("./", directory, sep="")
      isValidDirectory(dirPath)
      summ <- data.frame(id=numeric(), nobs=numeric())

      for(i in 1:length(id)){

        filePath <- paste(dirPath, "/", formatC(id[i], width = 3, flag = 0), ".csv", sep="")
        df <- read.csv(file = filePath, header = TRUE, sep=",", dec=".")

        newRow <- c(id[i], nrow(df[complete.cases(df),]))

        summ[nrow(summ)+1, ] <- newRow
      }
      summ

    },
    error = function(e)
    {
      print(e)
    }
  )
}

