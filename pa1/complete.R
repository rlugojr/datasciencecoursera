complete <- function(directory = "specdata", id = 1:332){
  #find files for selected monitors using id parameter

  allFiles <- c(1:332)
  csvfilenames <- paste(getwd(),directory,sprintf("%03d.csv", allFiles), sep = "/")
  #str(csvfilenames)

  csvfiles <- csvfilenames[id]
  #str(csvfiles)

  nobs <- numeric(length(id))
  #str(nobs)

  count = 0

  for (csvfile in csvfiles) {
      count <- count + 1
      data <- read.csv(csvfile)
      #str(data)

      nobs[count] <- nrow(data[complete.cases(data),])
      #print(nobs)
  }

  finalresults <- data.frame(id, nobs)
  finalresults
}
