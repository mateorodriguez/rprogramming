source('./best.R')
rankhospital <- function(state, outcome, num="best"){
  
  ## Read data
  outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Validate num param
  valid.num(outcome_df, num)
  
  ## Subsets the data by the given state and outcome and returns a data frame with
  ## - "State"
  ## - "Hospital.Name"
  ## - Outcome.Mortality
  outcome_df <- subset.by.outcome(outcome_df, state, outcome)
  
  ## Orders the data by the outcome rate
  outcome_df <- outcome_df[order(outcome_df[,3]),]
  
  if(num=="best")
    outcome_df <- subset(outcome_df, outcome_df[,3] == min(outcome_df[,3], na.rm = TRUE), 
                         select=c(1))
  else if(num=="worst")
    outcome_df <- subset(outcome_df, outcome_df[,3] == max(outcome_df[,3], na.rm = TRUE), 
                         select=c(1))
  else
      outcome_df <- outcome_df[num,]
    
  as.character(outcome_df[1,1])
}

valid.num <- function(data, num){
  
  if(num!= "best" && num!= "worst" && !is.numeric(num))
    stop("Invalid num")
  if(num > nrow(outcome_df))
    return(NA)
}