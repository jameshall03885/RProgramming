rankhospital <- function(istate, outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
  
  if ( outcome == "heart attack") outcome <- "Heart.Attack"
  else if ( outcome == "heart failure") outcome <- "Heart.Failure"
  else if ( outcome == "pneumonia") outcome <- "Pneumonia"
  else stop("invalid outcome")
  
  if ( !is.numeric(num) & ( !num %in% c("best","worst"))) stop("Invalid num argument")
  
  ## Read outcome data, interpret "Not Available" as NA
  all_outcomes <- read.csv("outcome-of-care-measures.csv"
                           , colClasses = "character", na.strings = "Not Available")
  
  ## select out State Hospitals with the desired outcome column.
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep="")
  
  if ( !(istate %in% state.abb) ) stop("invalid state")
  state_outcomes <- subset(all_outcomes, State == istate, select = c("Hospital.Name",outcome))
  
  ## prune out rows with missing data
  state_outcomes <- state_outcomes[complete.cases(state_outcomes),]
  #print(state_outcomes[,1:2])
  
  ## As I haven't figured out how to use a variable as a column name in filter expressions
  ## I'm renaming the second column to a fixed name.
  colnames(state_outcomes) <- c("Hospital.Name", "Rate")
  state_outcomes <- transform(state_outcomes, Rate = as.numeric(Rate))
  
  #str(state_outcomes)
  state_outcomes <- state_outcomes[order(state_outcomes[,"Rate"],state_outcomes[,"Hospital.Name"]),]
  #print(state_outcomes)

    if ( ! is.numeric(num)) {
    if ( identical(num,"best")) num = 1
    else num = nrow(state_outcomes)
  }

  winner <- state_outcomes[num,1]
  winner
}
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
#The function should check the validity of its arguments. If an invalid state value is passed to rankhospital, the function should throw an error via the stop function with the exact message “invalid state”. If an invalid outcome value is passed to rankhospital, the function should throw an error via the stop function with the exact message “invalid outcome”.
#Here is some sample output from the function.
#> source("rankhospital.R")
##[1] "DETAR HOSPITAL NAVARRO"
#> rankhospital("MD", "heart attack", "worst")
#3
##> rankhospital("MN", "heart attack", 5000)
#[1] NA