corr <- function(directory, threshold = 0){
  
  tryCatch(
    
    {
      dirPath <- paste("./", directory, sep="")
      isValidDirectory(dirPath)
      
      files <- list.files(path=dirPath, pattern = "*.csv", full.names = TRUE)
      
      csv_sulf <- data.frame(sulfate=numeric(0))
      csv_nit <- data.frame(nitrate=numeric(0))
      
      for(i in 1:length(files)){
        
        csv <- read.csv(file = files[i], header = TRUE, sep=",", dec=".")
        if(complete(directory, i)["nobs"] > threshold){
          
          csv_sulf <- rbind(csv_sulf, csv[complete.cases(csv),][c(2)])
          csv_nit <- rbind(csv_nit, csv[complete.cases(csv),][c(3)])
        }
        
      }
      cor(csv_sulf[[c(1)]], csv_nit[[c(1)]])
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