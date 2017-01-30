source('./rankhospital.R')
rankall - function(outcome, num = "best"){
  
  ## Read data
  outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Validate num
  valid.num(outcome_df, num)
  
  mydf <- data.frame()
  ## Run rankhospital.R function
  for(st in as.character(unique(outcome_df$State))){
    a <- rankhospital(st, outcome, num)
    
    
  }
  
  
}