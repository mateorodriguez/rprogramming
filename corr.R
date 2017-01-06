corr <- function(directory, threshold = 0){
  
  tryCatch(
    
    {
      dirPath <- paste(getwd(), "/", directory, sep="")
      isValidDirectory(dirPath)
      
      files <- list.files(path=dirPath, pattern = "*.csv", full.names = TRUE)
      
      
      
      
      
      
    }
    
    
    
    
  )
  
  
  
}

isValidDirectory <- function(directory){
  
  if(!dir.exists(directory))
    stop(call. = FALSE, "Specified directory does not exist.")
  
}