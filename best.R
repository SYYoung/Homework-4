best <- function(state, outcome){
 
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                         colClasses="character")
  
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
    ## clean up NA
    state_hosp_list[,match_list[outcome]] <- as.numeric(state_hosp_list[,match_list[outcome]])
    rank_list <- order(state_hosp_list[,match_list[outcome]], 
                     state_hosp_list$Hospital.Name,
                     na.last=NA)

    best_hosp_list <- state_hosp_list[rank_list[1],"Hospital.Name"]
    
    best_hosp_list
    
    ##best_list <- tapply(outcome_data[,11], outcome_data$State, min)
}

rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                           colClasses="character")
  
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
  
  ## convert the character inputs to numeric
  state_hosp_list[,match_list[outcome]] <- as.numeric(state_hosp_list[,match_list[outcome]])
  
  rank_list <- order(state_hosp_list[,match_list[outcome]], 
                     state_hosp_list$Hospital.Name,
                     na.last=NA)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  num_item <- NROW(rank_list)
  if (num == "best") {
    name_list <- state_hosp_list[rank_list[1],"Hospital.Name"]
  }
  else if (num == "worst"){
    name_list <- state_hosp_list[rank_list[num_item],"Hospital.Name"]
  }
  else if (num > num_item)
    name_list <- NA
  else {
    name_list <- state_hosp_list[rank_list[num],"Hospital.Name"]
  }
  name_list
}