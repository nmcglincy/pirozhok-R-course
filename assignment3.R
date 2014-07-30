# 2 Finding the best hospital in a state
# Write a function called best that take two arguments: 

# 1. the 2-character abbreviated name of a state 
# 2. and an outcome name. 

# The function reads the outcome-of-care-measures.csv and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. 

# The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).

best <- function(state, outcome) {
## Read outcome data
	hospital.data = read.csv("outcome-of-care-measures.csv",
		header = TRUE,
		colClasses = "character",
		na.strings = "Not Available")
# Only take the columns we need:
# 2 = Hospital Name
# 7 = State
# 11 = heart attack 30-day mortality rate
# 17 = heart failure 30-day mortality rate
# 23 = pneumonia  30-day mortality rate
	hospital.data = hospital.data[,c(2, 7, 11, 17, 23)]
## Check that state and outcome are valid
	if (sum(state %in% unique(hospital.data$State)) == 0) {
		stop("invalid state")
	}
	if (sum(outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
		stop("invalid outcome")
	}
## Return hospital name in that state with lowest 30-day death rate
	index = match(outcome, c("heart attack", "heart failure", "pneumonia"))
	state.hosp.data = na.omit(subset(hospital.data, State == state))[,c(1:2, (2 + index))]
	state.hosp.data = state.hosp.data[order(state.hosp.data$Hospital.Name),]
	state.hosp.data$Hospital.Name[which.min(as.numeric(state.hosp.data[,3]))]
}

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message \invalid outcome".
# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
# >
# 2
# Save your code for this function to a file named best.R.
