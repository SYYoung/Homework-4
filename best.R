best <- function(state, outcome){
 
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                         colClasses="character")
  print(unique(outcome_data$State))
  
  ## check that state and outcome are valid
  state_list <- unique(outcome_data$State)
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  match_list <-c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if (sum(state_list == state) == 0) {
    # the state name does not match
    stop("invalid state")
  }
  if (sum(outcome_list == outcome) == 0) {
    # the outcome name does not match 
    stop("invalid outcome")
  }
  
  ## based on the outcome, get the list
    state_hosp_list <- subset(outcome_data, State==state)
    min_val <- min(state_hosp_list[,match_list[outcome]])
    inc_list <- (state_hosp_list[,match_list[outcome]]==min_val)
    print(paste("inc list:",sum(inc_list)))
    best_hosp_list <- state_hosp_list[inc_list,]
    print(best_hosp_list[,"Hospital.Name"])
    
    ##best_list <- tapply(outcome_data[,11], outcome_data$State, min)

}

rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                           colClasses="character")
  print(unique(outcome_data$State))
  
  ## check that state and outcome are valid
  state_list <- unique(outcome_data$State)
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  match_list <-c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                 "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                 "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if (sum(state_list == state) == 0) {
    # the state name does not match
    stop("invalid state")
  }
  if (sum(outcome_list == outcome) == 0) {
    # the outcome name does not match 
    stop("invalid outcome")
  }
  
  ## based on the outcome, get the list
  state_hosp_list <- subset(outcome_data, State==state)
  rank_list <- sort(state_hosp_list[,match_list[outcome]])
  
  if (num == "best") {
    inc_list <- rank_list[1,]
  }
  else if (num == "worst"){
    inc_list <- rank_list[nrow(inc_list),]
  }
  else if (num > nrow(inc_list)) 
    stop("number is too large")
  else {
    inc_list <- rank_list[number,]
  }
  

  print(inc_list[,"Hospital.Name"])  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}