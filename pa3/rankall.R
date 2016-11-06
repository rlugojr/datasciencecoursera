#This function accepts 2 parameters: outcome type ("Heart Attack","Heart Failure","Pneumonia")
#and rank ("best", "worst",1,2,3...N). It returns the Hospital name and U.S. State for the hospital which
#has the 30-day outcome mortality rates for the specified outcome and at the rank specified by the user.

rankall <- function(outcomeName = NULL, num = 1L){

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

    #load dataset after validating parameters values
    #do not allow stringsAsFactors - the CSV has quotes surrounding number values.
    #Replace the strings in CSV file that should be NA
    obs <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = c("","Not Available"))

    #get valid values for Outcomes from loaded dataset
    Outcomes <- c("Heart Attack","Heart Failure","Pneumonia")

    #validate outcomeName parameter value against dataset
    if (!any(tolower(Outcomes) == tolower(outcomeName))) {
        stop("invalid outcome")
    }
    else {
        #set outcomeName to the valid value from the dataset.
        outcomeName = Outcomes[tolower(Outcomes) == tolower(outcomeName)]
    }

    # use outcomeName value to paste and set to the basename
    # of the dataset column which contains the chosen outcome scores
    outcomeCol = paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",outcomeName), sep = "")

    #create data frame from subset using State, hospital name and outcomeName columns
    hospital_outcome <- data.frame(obs["State"], obs["Hospital.Name"], obs[eval(outcomeCol)], stringsAsFactors = FALSE)

    #rename columns to replace awful, long column name and reduce typing
    colnames(hospital_outcome) <- c("State","Hospital","Score")

    #remove rows with NA values
    hospital_outcome <- na.omit(hospital_outcome)

    #Sort by State, Score, HospitalName
    hospital_outcome <- hospital_outcome[order(hospital_outcome$State, hospital_outcome$Score, hospital_outcome$Hospital),]

    #Get unique list of States from dataset for use in results
    StateNames <- unique(hospital_outcome$State)

    #create ID field and set sequential numbering for ranking purposes
    hospital_outcome$ID <- 1:nrow(hospital_outcome)
    #order dataset by the ID value
    hospital_outcome <- hospital_outcome[order(hospital_outcome$ID),]
    #Assign ranking values (1...N) to each row, beginning with 1 in each State group
    #round the ranking value and assign to "Rank"column in dataframe.
    hospital_outcome$Rank <- unlist(with(hospital_outcome, tapply(Score, State, function(x) round(rank(x,ties.method = "first")))))


    #maxrows = nrow(hospital_outcome)   #get total number of rows
    maxrows = nrow(hospital_outcome)

    #set default sortOrder to FALSE (ascending)
    sortOrder = FALSE
    #set rank value of 1 and sort order, ascending  is already set for "best"
    # and "descending" is used for "worst", if passed as an argument
    if (tolower(num) == "best") {
        #best is set to rank 1.
        rank <- 1L
    }
    else if (tolower(num) == "worst") {
        #worst is last ranked, but set rank to 1 and reverse sort, then grab first row
        rank <- 1L
        sortOrder = TRUE
    }
    else if (num > maxrows) {
        #if parameter value is greater than total rows then return NA
        return(NA)
    }
    else {
        #if none of the above apply, set rank = num parameter value given.
        rank <- as.integer(num)
    }

    #sort data subset using sortOrder
    sorted <- hospital_outcome[order(hospital_outcome$State, hospital_outcome$Score, decreasing = sortOrder),]
    #split subset into dataframe per State, limit the columns that are returned,
    #and then return a row from each State dataframe that is in the chosen rank position
    rankedResults <- do.call(rbind,lapply(split(sorted, sorted$State),"[",rank,c(2,1)))
    #Use States values as row names in order to replace the NA values that are returned.
    rankedResults$State <- StateNames
    #return the resulting set to the user.
    return(rankedResults)

}
