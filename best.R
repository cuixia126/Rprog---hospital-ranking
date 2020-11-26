best <- function(state, outcome) {
  library(dplyr)
  data_full <- read.csv("outcome-of-care-measures.csv")
  data <- data_full[, c(2, 7, 11, 17, 23)]
  names(data)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  data[, 3:5] <- lapply(data[, 3:5], as.numeric)
  data[, 2] <- as.factor(data[, 2])
  
  if (!is.element(state, data$State)) {stop ("invalid state")}
  if (!is.element(outcome, colnames(data))) {stop ("invalid outcome")}
  
  data_outcome <- select(data, c("Hospital.Name", "State", outcome))
  data_outcome_state <- filter(data_outcome, State == state)
  data_outcome_state <- na.omit(data_outcome_state)
  
  names(data_outcome_state) <- c("hospital", "state", "outcome")
  rank <- arrange(data_outcome_state, outcome, hospital)
  rank [1,1]
}