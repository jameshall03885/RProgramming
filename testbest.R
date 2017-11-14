testbest <- function() {
  res <- best("TX", "heart attack")
  print(res)
  
  ## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
  res <- best("TX", "heart failure")
  print(res)
  ## [1] "FORT DUNCAN MEDICAL CENTER"
  res <- best("MD", "heart attack")
  print(res)
  
  ## [1] "JOHNS HOPKINS HOSPITAL, THE"
  res <- best("MD", "pneumonia")
  print(res)
  
  ## [1] "GREATER BALTIMORE MEDICAL CENTER"
  res <- best("BB", "heart attack")
  print(res)
  
  ## Error in best("BB", "heart attack") : invalid state
  res <- best("NY", "hert attack")
  print(res)
  
  ## Error in best("NY", "hert attack") : invalid outcome
  ## >
}