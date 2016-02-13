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
    ## convert the list from character to numeric
    state_hosp_list[,match_list[outcome]] <- as.numeric(state_hosp_list[,match_list[outcome]])
    ## order the list based on the outcome, then alphabetical. The array rank_list stores
    ## the order, rank_list[1] has the smallest number of outcome
    rank_list <- order(state_hosp_list[,match_list[outcome]], 
                     state_hosp_list$Hospital.Name,
                     na.last=NA)

    best_hosp_list <- state_hosp_list[rank_list[1],"Hospital.Name"]
    best_hosp_list
}