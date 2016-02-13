rankStateHospital <- function(outcome_data, state, outcome, num) {
    match_list <-c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                   "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                   "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    ## based on the outcome, get the list
    state_hosp_list <- subset(outcome_data, State==state)
    print(state_hosp_list[,"Hospital.Name"])
    print(state_hosp_list[,match_list[outcome]])
    ## convert the character inputs to numeric
    state_hosp_list[,match_list[outcome]] <- as.numeric(state_hosp_list[,match_list[outcome]])
    
    rank_list <- order(state_hosp_list[,match_list[outcome]], 
                       state_hosp_list$Hospital.Name,
                       na.last=NA)
    print(paste("length of rank_list:",length(rank_list),rank_list))
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
    print(paste("class of name_list:",class(name_list), "name_list :",name_list))
    name_list
}


rankall <- function(outcome, num="best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",
                           colClasses="character")
  
  ## check that state and outcome are valid
  state_list <- sort(unique(outcome_data$State))
  print(state_list)
  outcome_list <- c("heart attack", "heart failure", "pneumonia")

  
  if (sum(outcome_list == outcome) == 0) {
    # the outcome name does not match 
    stop("invalid outcome")
  }
  
  ## for each state, find the hospital of the given rank
  all_state_list = list(Hospital.Name=character(length(state_list)), State=state_list)
  print("all_state_list")
  print(all_state_list)
  for (i in seq_along(state_list)) {
      all_state_list$Hospital.Name[i] = rankStateHospital(outcome_data,state_list[i],outcome, num)
      print(paste("i=",i,"all_state_list:",all_state_list$Hospital.Name[i] ))
  }
  
  all_state_list
}

