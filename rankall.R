rankall <- function (outcome,num="best"){
        #Read outcome data
        rank <- NULL
        ranked <- NULL
        states <- NULL
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        state_list <- unique(outcome_data[,7])
        valid_input <- c("heart attack","heart failure","pneumonia") #accepted values for outcome
        ##Check if outcome is valid
        if (!outcome %in% valid_input){
                stop("invalid outcome")
        }
        else {
                for (i in 1:length(state_list)) {
                        state_data <- outcome_data[outcome_data[,7] == state_list[i],] #outcome date for specific state (all columns)
                        if (outcome == "heart attack"){
                                if (num == "best"){
                                        rank <- sort(state_data[which.min(state_data[,11]),2])[1]
                                }
                                else if (num == "worst"){
                                        rank <- sort(state_data[which.max(state_data[,11]),2])[1]
                                }
                                else if (num > length(state_data)){
                                        rank <- NA
                                }
                                else{
                                        rank <- sort(state_data[which(state_data[,11] == sort(state_data[,11])[num]),2])[1]
                                }
                        }
                        else if (outcome == "heart failure"){
                                if (num == "best"){
                                        rank <- sort(state_data[which.min(state_data[,17]),2])[1]
                                }
                                else if (num == "worst"){
                                        rank <- sort(state_data[which.max(state_data[,17]),2])[1]
                                }
                                else if (num > length(state_data)){
                                        rank <- NA
                                }
                                else{
                                        rank <- sort(state_data[which(state_data[,17] == sort(state_data[,17])[num]),2])[1]
                                }                
                        }
                        else {
                                if (num == "best"){
                                        rank <- sort(state_data[which.min(state_data[,23]),2])[1]
                                }
                                else if (num == "worst"){
                                        rank <- sort(state_data[which.max(state_data[,23]),2])[1]
                                }
                                else if (num > length(state_data)){
                                        rank <- NA
                                }
                                else{
                                        rank <- sort(state_data[which(state_data[,23] == sort(state_data[,23])[num]),2])[1]
                                }
                        }
                #inside loop
                        states <- c(states,state_list[i])
                        ranked <- c(ranked,rank)
                        result <- data.frame(hospital=ranked,state=states)
                }
        }
        return(result)
}