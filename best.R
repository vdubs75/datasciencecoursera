best <- function (state,outcome){
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #reads in outcome data
        valid_input <- c("heart attack","heart failure","pneumonia") #accepted values for outcome
        ##Check the state and outcome are valid
        if (!state %in% outcome_data$State){
                stop("invalid state")
        }
        else if (!outcome %in% valid_input){
                stop("invalid outcome")
        }
        ##Return hospital name in that state with lowest 30-day death rate
        else {
                if (outcome == "heart attack"){
                        state_data <- outcome_data[outcome_data[,7] == state,]
                        hospital <- state_data[which.min(state_data[,11]),2]
                        }
                else if (outcome == "heart failure"){
                        state_data <- outcome_data[outcome_data[,7] == state,]
                        hospital <- state_data[which.min(state_data[,17]),2]
                        }
                else {
                        state_data <- outcome_data[outcome_data[,7] == state,]
                        hospital <- state_data[which.min(state_data[,23]),2]
                        } 
                return(sort(hospital)[1])
                }
        }