corr <- function(directory="specdata", threshold = 0){

  allFiles <- c(1:332)
  csvfiles <- paste(getwd(),directory,sprintf("%03d.csv", allFiles), sep = "/")

  nonaData <- data.frame(Date <- character(0), sulfate <- numeric(0), nitrate <- numeric(0), ID <- integer(0) )

  correlations <- numeric(0)

  for (csvfile in csvfiles) {
      data <- read.csv(csvfile)

      nonaData <- data[complete.cases(data),]
      #str(nonaData)

          if (threshold < nrow(nonaData) ) {
            result <- cor(nonaData$sulfate, nonaData$nitrate)
            correlations <- c(correlations, cor(nonaData$sulfate, nonaData$nitrate))
            #print(result)
          }
          else{
            #correlations <- c(correlations,0)
            #print(0)
          }
  }

  correlations
}
