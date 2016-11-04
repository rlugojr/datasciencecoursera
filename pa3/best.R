#This function accepts 2 parameters: state abbreviation and outcome type
#and returns the name of the hospital in the chosen state that
#has the best outcome value according to the 30-day mortality rates for the
#specified outcome.

best <- function(abbrState = NULL, outcomeName = NULL){

    #check if abbrState parameter has a value
    if (is.null(abbrState)) {
        stop("State parameter value is missing.")
    }

    #check if outcomeName parameter has a value
    if (is.null(outcomeName)) {
        stop("Outcome parameter is missing.")
    }

    #load dataset since parameters have values
    obs <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")

    #get valid values for States and Outcomes from dataset
    States <- unique(obs$State)
    Outcomes <- c("Heart Attack","Heart Failure","Pneumonia")

    #validate State parameter value against dataset
    if (!any(States == toupper(abbrState))) {
        stop("invalid state")
    }
    else {
        #set value to uppercase
        abbrState = toupper(abbrState)
    }

    #validate outcomeName parameter value against dataset
    if (!any(tolower(Outcomes) == tolower(outcomeName))) {
        stop("invalid outcome")
    }
    else {
      #set value to valid value format.
      outcomeName = Outcomes[tolower(Outcomes) == tolower(outcomeName)]
    }

    #get subset of dataset by State parameter value
    state_obs <- subset(obs, State == abbrState)

    #set name of dataset column using outcomeName value
    outcomeCol = paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",outcomeName), sep = "")

    #create data frame limited to the hospital name and outcomeName columns
    hospital_outcome <- data.frame(state_obs["Hospital.Name"], state_obs[eval(outcomeCol)], stringsAsFactors = FALSE)

    #remove rows with incomplete cases
    hospital_outcome <- na.omit(hospital_outcome)

    #cast data value column to numeric
    hospital_outcome[,2] <- as.numeric(hospital_outcome[,2])

    #get MIN outcomeName value as indicator.
    best_hospital_ind <- min(hospital_outcome[,2])

    #get row(s) with the indicated MIN value
    hospitalData <- subset(hospital_outcome, hospital_outcome[,2] == best_hospital_ind)

    #create vector with resulting hospital names
    hospitals <- hospitalData$Hospital.Name

    #sort resulting hospital names
    result = sort(hospitals)

    #display the top result
    return(result[1])

}
