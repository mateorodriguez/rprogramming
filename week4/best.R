## Finds the best hospital in a state by given outcome
best <- function(state, outcome){
  
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Validate if params are valid
  validate.params(outcome_df, state, outcome)
  
  ## Get data from the specified state
  ## Get data according to given outcome: {heart attack, heart failure, pnemonia}
  ## Orders the data by the hospital name (Hospital.Name)
  outcome_df <- subset.by.outcome(outcome_df, state, outcome)
  
  ## Get the hospitals' names with mortality rates equal to the
  ## minimun mortality rate of all hospitals in the data frame
  outcome_df <- subset(outcome_df, outcome_df[,3] == min(outcome_df[,3], na.rm = TRUE), 
                       select=c(1))
  
  ## Gets the first hospital's name. Useful if there is more than 1 (one)
  ## hospital that fits the minimun mortality rate.
  as.character(outcome_df[1,])
  
}

subset.by.outcome <- function(data, state, outcome){
  
  ## Get data from the specified state
  data <- subset(data, State==state)
  
  ## Get data according to given outcome: {heart attack, heart failure, pnemonia}
  data <- select.outcome(data, outcome)
  
  ## Orders the data by the hospital name (Hospital.Name)
  data <- data[order(data$Hospital.Name),]
  
  data
}

## Validate if given parameteres are valid
validate.params <- function(data, state, outcome){
  
  ## Param 'state' is valid if it exists in data column 'State' 
  if( !(state %in% data$State ) )
    stop("Invalid state")
  
  ## Valid outcome variables
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  
  ## Param 'outcome' is valid if exists in validOutcome vector
  if(!(outcome %in% validOutcome))
    stop("Invalid outcome")
}

## Subsets data by given outcome and selects the necessary columns:
## - "State"
## - "Hospital.Name"
## - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
## - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
## - "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
select.outcome <- function(data, outcome){
  
  data_subsetted <- data
  if(outcome=="heart attack")
    data_subsetted <- subset(data, select=c(2,7,11))
  if(outcome=="heart failure")
    data_subsetted <- subset(data, select=c(2,7,17))
  if(outcome=="pneumonia")
    data_subsetted <- subset(data, select=c(2,7,23))
  
  data_subsetted
}