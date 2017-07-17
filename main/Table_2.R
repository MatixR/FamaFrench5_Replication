################################################################################
###The following code replicates Table 1 
################################################################################


## Call libraries
library(vars)


## Functions

SpliceByDate <- function(date1, date2, data) { 
  # gets data between two given dates
  data[data$date >= date1 & data$date <= date2,]
}

averageExcess <- function(datapath, dataRiskFree, startDate, endDate) {
  # Finds the average excess returns of a portfolio given a risk free rate
  #
  # Args:
  #   datapath: location of file containing portfolio return data in .csv
  #   dataFactors: data frame containing risk free rate as 'RF'
  #   startDate: beginning date to average from
  #   endDate: end date of averages
  #
  # Returns:
  #   data frame containing average excess returns
  
  # Load raw data
  dataPortfolio <- read.csv(file=datapath, header=TRUE, sep=",")
  
  # Splice relevant from dataPortfolio
  dataPortfolio$date <- as.Date(paste(dataPortfolio$date, "01"), format = "%Y%m %d")
  dataPortfolio      <- SpliceByDate(startDate, endDate, dataPortfolio)
  
  # Splice relevant from dataFactors
  dataFactors$date <- as.Date(paste(dataFactors$date, "01"), format = "%Y%m %d")
  dataFactors      <- SpliceByDate(startDate, endDate, dataFactors)
  
  # Averages 
  results <- data.frame(row.names = c("mean"))
  
  for (i in colnames(dataPortfolio[2:ncol(dataPortfolio)])) {
    
    #if (grepl("SMALL", i) || grepl("BIG", i)) {
      
    # Average excess returns
    average <- mean(as.numeric(unlist(dataPortfolio[i]-dataFactors["RF"])))
    
    # Add average to results
    results[i] <- average
    
  }
  
  # Formatting
  results <- t(results)
  row.names(results) <- gsub("ME1", "SMALL", row.names(results))
  row.names(results) <- gsub("ME2", "BIG",   row.names(results))
  results
  
}


## Factor Data

# Get 5 Factor Returns Data
datapathF   <- "../data/returns/5_Factors.csv"
dataFactors <- read.csv(file=datapathF, header=TRUE, sep=",")


## Main

# Relevant dates for table 1
startDate <- as.Date("July 01 1963", format = "%B %d %Y")
endDate   <- as.Date("December 01 2013", format = "%B %d %Y")

# Panel A results
datapathA <- "../data/trivariate_sorts/32_SIZE_BM_OP.csv"
resultsA  <- averageExcess(datapathA, dataFactors, startDate, endDate)

# Panel B results
datapathB <- "../data/trivariate_sorts/32_SIZE_BM_INV.csv"
resultsB  <- averageExcess(datapathB, dataFactors, startDate, endDate)

# Panel C results
datapathC <- "../data/trivariate_sorts/32_SIZE_OP_INV.csv"
resultsC  <- averageExcess(datapathC, dataFactors, startDate, endDate)


