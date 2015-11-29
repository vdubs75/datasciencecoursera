rankhospital <- function (state,outcome,num="best"){
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #reads in outcome data
        heart_attack <- as.numeric(outcome_data[,11])  #stores heart attack as numeric value
        heart_failure <- as.numeric(outcome_data[,17]) #stores heart failure as numeric value
        pneumonia <- as.numeric(outcome_data[,23]) #stores pneumonia as numeric value
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
                        state_data <- outcome_data[outcome_data[,7] == state,] #outcome date for specific state (all columns)
                        if (num == "best"){
                                hospital <- sort(state_data[which.min(state_data[,11]),2])[1]
                        }
                        else if (num == "worst"){
                                hospital <- sort(state_data[which.max(state_data[,11]),2])[1]
                        }
                        else if (num > length(state_data)){
                                hospital <- NA
                        }
                        else{
                                hospital <- state_data[which(state_data[,11] == sort(state_data[,11])[num]),2]
                        }
                }
                else if (outcome == "heart failure"){
                        state_data <- outcome_data[outcome_data[,7] == state,]
                        if (num == "best"){
                                hospital <- sort(state_data[which.min(state_data[,17]),2])[1]
                        }
                        else if (num == "worst"){
                                hospital <- sort(state_data[which.max(state_data[,17]),2])[1]
                        }
                        else if (num > length(state_data)){
                                hospital <- NA
                        }
                        else{
                                hospital <- state_data[which(state_data[,17] == sort(state_data[,17])[num]),2]
                        }                }
                else {
                        state_data <- outcome_data[outcome_data[,7] == state,]
                        if (num == "best"){
                                hospital <- sort(state_data[which.min(state_data[,23]),2])[1]
                        }
                        else if (num == "worst"){
                                hospital <- sort(state_data[which.max(state_data[,23]),2])[1]
                        }
                        else if (num > length(state_data)){
                                hospital <- NA
                        }
                        else{
                                hospital <- state_data[which(state_data[,23] == sort(state_data[,23])[num]),2]
                        }                } 
                return(hospital[1])
        }
}