
##
## > source("best.R")
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome
## >
##

best <- function(istate, outcome) {
  
  ## Check that state and outcome are valid

  ## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
 
  if ( outcome == "heart attack") outcome <- "Heart.Attack"
  else if ( outcome == "heart failure") outcome <- "Heart.Failure"
  else if ( outcome == "pneumonia") outcome <- "Pneumonia"
  else stop("invalid outcome")
  
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
  best_rate <- min(state_outcomes[,"Rate"])

  finalists <-filter(state_outcomes, Rate == best_rate)
  #print(finalists)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate  TODO: Handle multiple minimum matches by lexigraphical sorting
  winner <- finalists[1,"Hospital.Name"]
  winner
}
