rankhospital <- function(state, outcome, num = "best") {
  library(dplyr)
  data_full <- read.csv("outcome-of-care-measures.csv")
  data <- data_full[, c(2, 7, 11, 17, 23)]
  names(data)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  data[, 3:5] <- lapply(data[, 3:5], as.numeric)
  if (!is.element(state, data$State)) {stop ("invalid state")}
  if (!is.element(outcome, colnames(data))) {stop ("invalid outcome")}
  
  data_outcome <- select(data, c("Hospital.Name", "State", outcome))
  data_outcome_state <- filter(data_outcome, State == state)
  data_outcome_state <- na.omit(data_outcome_state)
  
  names(data_outcome_state) <- c("hospital", "state", "outcome")
  rank <- arrange(data_outcome_state, outcome, hospital)
  if (num == "best"){rank[1,1]}
  else if (num == "worst") {rank[nrow(rank), 1]}
  else {num <- as.numeric(num)
  rank[num, 1]}
}