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
  
  ## if outcome is heart.attack
  if (outcome == "heart attack") {
    state_hosp_list <- subset(outcome_data, State==state)
    min_val <- min(state_hosp_list[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
    inc_list <- (state_hosp_list[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]==min_val)
    print(paste("inc list:",sum(inc_list)))
    best_hosp_list <- state_hosp_list[inc_list,]
    print(best_hosp_list[,"Hospital.Name"])
    
    ##best_list <- tapply(outcome_data[,11], outcome_data$State, min)
  }

}
