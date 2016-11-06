#This function accepts 3 parameters: state abbreviation,outcome type
#and rank. It returns the name of the hospital in the chosen state which
#has the best 30-day outcome value according to the mortality rates for the
#specified outcome and at the rank specified by the user.

rankhospital <- function(abbrState = NULL, outcomeName = NULL, num = 1L){

    #check if abbrState parameter has a value
    if (is.null(abbrState)) {
        stop("State parameter value is missing.")
    }

    #check if outcomeName parameter has a value
    if (is.null(outcomeName)) {
        stop("Outcome parameter is missing.")
    }

    #check for valid num parameter value
    if (!is.numeric(num)) {
        if (!tolower(num) == "best") {
            if (!tolower(num) == "worst") {
                stop("Num parameter is invalid")
            }
        }
    }


    #load dataset because parameters have values
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

    #get subset using State parameter value
    state_obs <- subset(obs, State == abbrState)

    #set name of dataset column using outcomeName value
    outcomeCol = paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",outcomeName), sep = "")

    #create dataframe limited to the hospital name and outcomeName columns
    hospital_outcome <- data.frame(state_obs["Hospital.Name"], state_obs[eval(outcomeCol)], stringsAsFactors = FALSE)

    #replace columns names for readability and to reduce typing
    colnames(hospital_outcome) <- c("HospitalName","Score")

    #remove rows with incomplete cases
    hospital_outcome <- na.omit(hospital_outcome)

    #add column for ranking hospitals by score
    hospital_outcome$rank <- NA

    #order data by Score and Hospital
    hospital_outcome <- hospital_outcome[order(hospital_outcome$Score, hospital_outcome$HospitalName),]

    #create ranking values
    hospital_outcome$rank <- round(rank(hospital_outcome$Score))

    #get total number of rows
    maxrows = nrow(hospital_outcome)

    #set rank values for best or worst if passed as an argument
    if (tolower(num) == "best") {
      #best is 1 on our scale
      rank = 1L
    }
    else if (tolower(num) == "worst") {
      #worst is last row.
      rank = as.integer(maxrows)
    }
    else if (num > maxrows) {
      #if parameter value is greater than total rows then return NA
      return(NA)
    }
    else {
      #if non of the above apply, set rank = number given.
      rank = as.integer(num)
    }

    #retrieve and display the ranked result
    rankResult <- hospital_outcome[rank,]
    return(rankResult$HospitalName)

}
