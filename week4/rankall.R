source('./rankhospital.R')
rankall <- function(outcome, num = "best"){
  
  ## Read data
  outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Validate num
  valid.num(outcome_df, num)
  
  ## Create data.frame
  ranking <- data.frame(hospital=character(), state=character(), stringsAsFactors = FALSE)
  
  ## Run rankhospital.R function for every State
  for(st in sort(as.character(unique(outcome_df$State)))){
    
    newrow <- c(rankhospital(st, outcome, num), st)
    ranking[nrow(ranking)+1, ] <- newrow
    
  }
  
  ranking
  
}