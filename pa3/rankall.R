#This function accepts 3 parameters: state abbreviation,outcome type
#and rank. It returns the name of the hospital in the chosen state which
#has the best 30-day outcome value according to the mortality rates for the
#specified outcome and at the rank specified by the user.

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

  #load dataset since parameters have values
  obs <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = c("","Not Available"))

  #get valid values for Outcomes from dataset
  Outcomes <- c("Heart Attack","Heart Failure","Pneumonia")

  #validate outcomeName parameter value against dataset
  if (!any(tolower(Outcomes) == tolower(outcomeName))) {
    stop("invalid outcome")
  }
  else {
    #set value to valid value format.
    outcomeName = Outcomes[tolower(Outcomes) == tolower(outcomeName)]
  }

  #set name of dataset column using outcomeName value
  outcomeCol = paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",outcomeName), sep = "")

  #create data frame limited to the State, hospital name and outcomeName columns
  hospital_outcome <- data.frame(obs["State"], obs["Hospital.Name"], obs[eval(outcomeCol)], stringsAsFactors = FALSE)

  #rename columns to replace awful, long column name
  colnames(hospital_outcome) <- c("State","HospitalName","Score")

  #remove rows with incomplete cases
  hospital_outcome <- na.omit(hospital_outcome)

  #add column for ranking hospitals by score
  hospital_outcome$rank <- NA

  #Sort by State, Score, HospitalName
  hospital_outcome <- hospital_outcome[order(hospital_outcome$State, hospital_outcome$Score, hospital_outcome$HospitalName),]

  #hospital_outcome$rank <- unlist(with(hospital_outcome, tapply(Score,HospitalName,State,rank)))
  #hospital_outcome$rank <- 1:nrow(hospital_outcome)
  transform(hospital_outcome, rank <- ave(hospital_outcome$Score, hospital_outcome$State, FUN = function(x) rank(x, ties.method = "first")))
  return(head(hospital_outcome))

  #set rank values for best or worst if passed as an argument
  #maxrows = nrow(hospital_outcome)   #get total number of rows
  #if (tolower(num) == "best") {
    #best is 1 on our scale
  #  rank = 1L
  #}
  #else if (tolower(num) == "worst") {
    #worst is last row.
  #  rank = as.integer(maxrows)
  #}
  #else if (num > maxrows) {
    #if parameter value is greater than total rows then return NA
  #  return(NA)
  #}
  #else {
    #if non of the above apply, set rank = number given.
  #  rank = as.integer(num)
  #}
  #rank

  #retrieve and display the ranked result
  #rankResult <- hospital_outcome[rank,]
  #rankResult

}
