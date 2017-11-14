rankall <- function(outcome, inum = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
  if ( outcome == "heart attack") outcome <- "Heart.Attack"
  else if ( outcome == "heart failure") outcome <- "Heart.Failure"
  else if ( outcome == "pneumonia") outcome <- "Pneumonia"
  else stop("invalid outcome")
  
  if ( !is.numeric(inum) & ( !inum %in% c("best","worst"))) stop("Invalid num argument")
  
  ## Read outcome data, interpret "Not Available" as NA
  all_outcomes <- read.csv("outcome-of-care-measures.csv"
                           , colClasses = "character", na.strings = "Not Available")
  
  ## select out State Hospitals with the desired outcome column.
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep="")
  
  outcomes <- subset(all_outcomes, select = c("Hospital.Name","State",outcome))
  
  ## prune out rows with missing data
  outcomes <- outcomes[complete.cases(outcomes),]
  #print(state_outcomes[,1:2])
  
  ## As I haven't figured out how to use a variable as a column name in filter expressions
  ## I'm renaming the second column to a fixed name.
  colnames(outcomes) <- c("Hospital", "State", "Rate")
  outcomes <- transform(outcomes, Rate = as.numeric(Rate))
  
  #str(state_outcomes)
  outcomes <- outcomes[order(outcomes[,"State"],outcomes[,"Rate"],outcomes[,"Hospital"]),]
  #print(outcomes)
  
  all_rank <- c()
  all_rates <- c()
  outcomes <- split(outcomes,f = outcomes$State)
  for ( st in state.abb ) {
    if (is.numeric(inum)) {
      num <- inum
    }else {
      #print("inum is not numeric ...")
      if ( identical(inum,"best")) {
        num <- 1
        #print("num is best")
      }
      if ( identical(inum,"worst")) {
        #print("num is worst")
        num <- nrow(outcomes[[st]])
      }
    }
    #print(num)
    st_rank <- outcomes[[st]][num,"Hospital"]
    st_rate <- outcomes[[st]][num,"Rate"]
    #print (st_rank)
    all_rank <- c( all_rank, st_rank )
    all_rates <- c( all_rates, st_rate)
  }
  result <- data.frame(state.abb,all_rank,all_rates)
  colnames(result) <- c("state","hospital","rate")
  result
}