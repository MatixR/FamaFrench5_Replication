################################################################################
### The following code replicates Table 1 with rolling regressions 
################################################################################


## Call libraries
library(vars)


## Functions

SpliceByDate <- function(date1, date2, data) { 
  # gets data between two given dates
  data[data$date >= date1 & data$date <= date2,]
}

regressFactorsRolling <- function(datapath, dataFactors, factors, 
                                  startDate, endDate, window) {
  # Performs a rolling regression on portfiolio return data between certain 
  # dates using a given list of factors
  #
  # Args:
  #   datapath: location of file containing portfolio return data in .csv
  #   dataFactors: data frame containing returns of factors
  #   factors: names of factors used in regression as a vector string
  #   startDate: beginning date to regress from
  #   endDate: end date of regressions
  #   window: rolling regression window
  #
  # Returns:
  #   list containing results of rolling OLS

  # Load raw data
  dataPortfolio <- read.csv(file=datapath, header=TRUE, sep=",")
  
  # Splice relevant from dataPortfolio
  dataPortfolio$date <- as.Date(paste(dataPortfolio$date, "01"), format = "%Y%m %d")
  dataPortfolio      <- SpliceByDate(startDate, endDate, dataPortfolio)

  # Splice relevant from dataFactors
  dataFactors$date <- as.Date(paste(dataFactors$date, "01"), format = "%Y%m %d")
  dataFactors      <- SpliceByDate(startDate, endDate, dataFactors)
  
  # Regressions 
  results         <- list()
  portfolio_names <- colnames(dataPortfolio[2:ncol(dataPortfolio)])
  
  for (j in c(1:length(portfolio_names))) {
    
    i = portfolio_names[j]
    
    # Regression vars
    x <- dataFactors[factors]
    y <- dataPortfolio[i]-dataFactors["RF"]
    temp <- data.frame(y, x, row.names = dataFactors$date)
    colnames(temp)[1] <- "y"

    # Rolling regression on excess returns
    fit <- rollapply(zoo(temp), 60, 
                     FUN = function(Z) { 
                       coef(lm(y ~ ., data = as.data.frame(Z)))
                     }, by.column = FALSE, align = "right"
    )
    
    index(fit) <- dataFactors$date[window:nrow(dataFactors)]
    
    results[[i]] <- fit 
    
  }
  
  return(results)
  
}


## Factor Data

# Get 5 Factor Returns Data
datapathF   <- "../data/returns/5_Factors.csv"
dataFactors <- read.csv(file=datapathF, header=TRUE, sep=",")


## Main

# Params
startDate <- as.Date("July 01 1963", format = "%B %d %Y")
endDate   <- as.Date("December 01 2013", format = "%B %d %Y")
factors   <- c("Mkt.RF", "SMB", "HML", "RMW", "CMA")
window    <- 60

# Panel A results
datapathA <- "../data/bivariate_sorts/25_SIZE_BM.csv"
resultsA  <- regressFactorsRolling(datapathA, dataFactors, factors, 
                            startDate, endDate, window)

# Panel B results
datapathB <- "../data/bivariate_sorts/25_SIZE_OP.csv"
resultsB  <- regressFactorsRolling(datapathB, dataFactors, factors, 
                                   startDate, endDate, window)

# Panel C results
datapathC <- "../data/bivariate_sorts/25_SIZE_INV.csv"
resultsC  <- regressFactorsRolling(datapathC, dataFactors, factors, 
                                   startDate, endDate, window)


## Plots

pdf("../results/Table_1_rolling.pdf")

# Panel A
for (sort in c(1:25)) {
  
  plotname <- paste(c("Panel A: ", names(resultsA)[sort]), collapse = "")
  plot(resultsA[[sort]], main = plotname, xlab = "date" )
  
}

# Panel B
for (sort in c(1:25)) {
  
  plotname <- paste(c("Panel B: ", names(resultsB)[sort]), collapse = "")
  plot(resultsB[[sort]], main = plotname, xlab = "date" )
  
}

# Panel C
for (sort in c(1:25)) {
  
  plotname <- paste(c("Panel C: ", names(resultsC)[sort]), collapse = "")
  plot(resultsC[[sort]], main = plotname, xlab = "date" )
  
}

dev.off()






