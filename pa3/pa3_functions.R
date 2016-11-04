best <- function(abbrState = NULL, outcomeName = NULL){

    if (is.null(abbrState)) {
        stop("State parameter value is missing.")
    }

    if (is.null(outcomeName)) {
        stop("Outcome parameter is missing.")
    }

    obs <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")

    States <- unique(obs$State)

    Outcomes <- c("Heart Attack","Heart Failure","Pneumonia")

    if (!any(States == toupper(abbrState))) {
        stop("invalid state")
    }
    else {
        abbrState = toupper(abbrState)
    }

    if (!any(tolower(Outcomes) == tolower(outcomeName))) {
        stop("invalid outcome")
    }
    else {
      outcomeName = Outcomes[tolower(Outcomes) == tolower(outcomeName)]
    }

    state_obs <- subset(obs, State == abbrState)

    outcomeCol = paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",outcomeName), sep = "")

    hospital_outcome <- data.frame(state_obs["Hospital.Name"], state_obs[eval(outcomeCol)], stringsAsFactors = FALSE)
    hospital_outcome <- na.omit(hospital_outcome)

    hospital_outcome[,2] <- as.numeric(hospital_outcome[,2])

    best_hospital_ind <- min(hospital_outcome[,2])

    hospitalData <- subset(hospital_outcome, hospital_outcome[,2] == best_hospital_ind)
    hospitals <- hospitalData$Hospital.Name
    result = sort(hospitals)
    result[1]
    #sort(c(result$Hospital.Name, result[,2]))

}
