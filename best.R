best <- function(state, outcome){
 
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                         colClasses="character")
  print(unique(outcome_data$State))
  
  ## check that state and outcome are valid
  state_list <- unique(outcome_data$State)
  outcome_list <- c("heart failure", "heart attack", "pneumonia")
  
  if (sum(state_list == state) == 0) {
    # the state name does not match
    stop("invalid state")
  }
  if (sum(outcome_list == outcome) == 0) {
    # the outcome name does not match 
    stop("invalid outcome")
  }

}
