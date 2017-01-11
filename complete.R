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

isValidDirectory <- function(directory){
  
  if(!dir.exists(directory))
    stop(call. = FALSE, "Specified directory does not exist.")
  
}