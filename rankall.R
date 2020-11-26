rankall <- function(outcome, num = "best") {
  library(dplyr)
  data_full <- read.csv("outcome-of-care-measures.csv")
  data <- data_full[, c(2, 7, 11, 17, 23)]
  names(data)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  data[, 3:5] <- lapply(data[, 3:5], as.numeric)
  if (!is.element(outcome, colnames(data))) {stop ("invalid outcome")}
  
  data_outcome <- select(data, c("Hospital.Name", "State", outcome))
  data_outcome <- na.omit(data_outcome)
  names(data_outcome) <- c("hospital", "state", "outcome")
  state_list <- unique(data_outcome$state)
  data_outcome_state <- split(data_outcome, data_outcome$state)
  data_outcome_state <- lapply(data_outcome_state, arrange, outcome, hospital)                             
  
  if (num == "best") {
    results <- data.frame()
    for (i in state_list) {result <- data_outcome_state[[i]][1, 1:2]
    results <- rbind(results, result)
    }
    results <- arrange(results, state)
    results
    }
  else if (num == "worst") {
    results <- data.frame()
    for (i in state_list) {result <- 
      data_outcome_state[[i]][nrow(data_outcome_state[[i]]), 1:2]
    results <- rbind(results, result)
    }
    row.names(results) <- 1: nrow(results)
    results <- arrange(results, state)
    results
  }
  else {
    results <- data.frame()
    for (i in state_list) {result <- data_outcome_state[[i]][num, 1:2]
    results <- rbind(results, result)
    }
    row.names(results) <- 1: nrow(results)
    results <- arrange(results, state)
    results
  }
}