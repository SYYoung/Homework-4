rankStateHospital <- function(outcome_data, state, outcome, num) {
    match_list <-c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                   "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                   "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    ## based on the outcome, get the list
    state_hosp_list <- subset(outcome_data, State==state)
 
    ## convert the character inputs to numeric
    state_hosp_list[,match_list[outcome]] <- as.numeric(state_hosp_list[,match_list[outcome]])
    
    ## order the list: first based on the outcome value, then the alphabetical order
    ## rank_list[1] stores the lowest number of outcome value
    rank_list <- order(state_hosp_list[,match_list[outcome]], 
                       state_hosp_list$Hospital.Name,
                       na.last=NA)
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


rankall <- function(outcome, num="best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                           colClasses="character")
  
  ## check that state and outcome are valid
  state_list <- sort(unique(outcome_data$State))
  outcome_list <- c("heart attack", "heart failure", "pneumonia")

  
  if (sum(outcome_list == outcome) == 0) {
    # the outcome name does not match 
    stop("invalid outcome")
  }
  
  ## for each state, find the hospital of the given rank
  hosp_list <- character(length(state_list))
  all_state_list = list(Hospital.Name=character(length(state_list)), State=state_list)
  for (i in seq_along(state_list)) {
      hosp_list[i] = rankStateHospital(outcome_data,state_list[i],outcome, num)
  }
  
  all_state_list <- cbind(hosp_list, state_list)
  
  all_state_list
}

