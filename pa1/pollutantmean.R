pollutantmean <- function(directory="specdata", pollutant, id = 1:332){
    #find files for selected monitors using id parameter
    allFiles <- c(1:332)
    #print(allFiles)

    csvfilenames <- paste(getwd(),directory,sprintf("%03d.csv", allFiles), sep = "/")
    #str(csvfilenames)

    csvfiles <- csvfilenames[id]
    #str(csvfiles)

    pollutantData <- data.frame(Date <- character(0), sulfate <- numeric(0), nitrate <- numeric(0), ID <- integer(0) )

    for (csvfile in csvfiles) {
      data <- read.csv(csvfile)
      pollutantData <- rbind(pollutantData,data)

      #pollutantData <- mean(pollutantCases)
    }
    #View(pollutantData)
    pollutantCases <- pollutantData[complete.cases(pollutantData[pollutant]),]
    pollutantValues <- as.vector(pollutantCases[[pollutant]])
    str(pollutantValues)
    pollutantMean <- mean(pollutantValues)
    pollutantMean

}
