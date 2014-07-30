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

##########################################################

# 3 Ranking hospitals by outcome in a state

# Write a function called rankhospital that takes three arguments: 

# the 2-character abbreviated name of a state (state),
# an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (num).

# The function reads the outcome-of-care-measures.csv le and returns a character vector with the name
# of the hospital that has the ranking specied by the num argument. 

# For example, the call

# rankhospital("MD", "heart failure", 5)

# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. 

# The num argument can take values \best", \worst", or an integer indicating the ranking
# (smaller numbers are better). 

# If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. 

# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (\TX"),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
# (8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
# scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
# manner (i.e. where one vector is used to break ties in another vector).

# The function should use the following template.
rankhospital <- function(state, outcome, num = "best") {
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
## Return hospital name in that state with the given rank
## 30-day death rate
	index = match(outcome, c("heart attack", "heart failure", "pneumonia"))
	state.hosp.data = na.omit(subset(hospital.data, State == state))[,c(1:2, (2 + index))]
	state.hosp.data = state.hosp.data[order(state.hosp.data$Hospital.Name),]
	state.hosp.data$Rank = rank(as.numeric(state.hosp.data[,3]), ties.method = "first")
	if (num == "best") {
			state.hosp.data$Hospital.Name[which.min(as.numeric(state.hosp.data[,3]))]
		} else if (num == "worst") {
			state.hosp.data$Hospital.Name[which.max(as.numeric(state.hosp.data[,3]))]
		} else if (num >= min(state.hosp.data$Rank) & num <= max(state.hosp.data$Rank)) {
			state.hosp.data$Hospital.Name[which(state.hosp.data$Rank == num)]
		} else print("NA")
}

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message \invalid outcome".

# Here is some sample output from the function.
# 3
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
 rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
> rankhospital("MN", "heart attack", 5000)
# [1] NA
# Save your code for this function to a le named rankhospital.R.
