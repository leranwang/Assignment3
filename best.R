##Write a function called best
#that take two arguments:  the 2-character abbreviated name of a state and an
#outcome name.  The function reads theoutcome-of-care-measures.csv
#le and returns a character vectorwith  the  name  of  the  hospital  that  has  the  best  (i.e.   lowest)  30-day  mortality  for  the  specied  outcome
#in that state.  The hospital name is the name provided in the Hospital.Name variable.  The outcomes can be one of \heart attack", \heart failure", or \pneumonia".  Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
#Handling ties.  If there is a tie for the best hospital for a given outcome, then the hospital names should be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e.  if hospitals \b", \c",
#and \f" are tied for best, then hospital \b" should be returned).
#The function should use the following template.
#best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
#}
#The function should check the validity of its arguments.  If an invalid state value is passed to
#best,  the function should throw an error via the stop function with the exact message \invalid state".  If an invalid
#outcome value is passed to best , the function should throw an error via the
#stop function with the exact message \invalid outcome".
#Here is some sample output from the function.
#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#>
#Save your code for this function to a le named best.R

## best.R

best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  #"./" means the workplace
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  idx <- which.min(as.double(data.state[,colName]))
  data.state[idx,"Hospital.Name"]
}
