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

regressFactors <- function(datapath, dataFactors, factors, startDate, endDate) {
  # Regresses on portfiolio return data between certain dates using a given 
  # list of factors
  #
  # Args:
  #   datapath: location of file containing portfolio return data in .csv
  #   dataFactors: data frame containing returns of factors
  #   factors: names of factors used in regression as a vector string
  #   startDate: beginning date to regress from
  #   endDate: end date of regressions
  #
  # Returns:
  #   data frame containing results of OLS
  
  # Load raw data
  dataPortfolio <- read.csv(file=datapath, header=TRUE, sep=",")
  
  # Splice relevant from dataPortfolio
  dataPortfolio$date <- as.Date(paste(dataPortfolio$date, "01"), format = "%Y%m %d")
  dataPortfolio      <- SpliceByDate(startDate, endDate, dataPortfolio)
  
  # Splice relevant from dataFactors
  dataFactors$date <- as.Date(paste(dataFactors$date, "01"), format = "%Y%m %d")
  dataFactors      <- SpliceByDate(startDate, endDate, dataFactors)
  
  # Regressions 
  results <- data.frame(row.names = c("r^2","intercept", factors))
  
  for (i in colnames(dataPortfolio[2:ncol(dataPortfolio)])) {
    
    # OLS on excess returns
    fit <- lm(as.numeric(unlist(dataPortfolio[i]-dataFactors["RF"])) ~ ., 
              data = dataFactors[factors])
    
    # Add regression to results
    results[i] <- c(summary(fit)$r.squared, summary(fit)$coefficients[1:6])  
    
  }
  
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

# Relevant factors
factors  <- c("Mkt.RF", "SMB", "HML", "RMW", "CMA")

# Panel A results
datapathA <- "../data/bivariate_sorts/25_SIZE_BM.csv"
resultsA  <- regressFactors(datapathA, dataFactors, factors, startDate, endDate)

# Panel B results
datapathB <- "../data/bivariate_sorts/25_SIZE_OP.csv"
resultsB  <- regressFactors(datapathB, dataFactors, factors, startDate, endDate)

# Panel C results
datapathC <- "../data/bivariate_sorts/25_SIZE_INV.csv"
resultsC  <- regressFactors(datapathC, dataFactors, factors, startDate, endDate)


## Plots

pdf("../results/Table_1_plots.pdf")

names <- c()
for (i in c(1:5)) {
  for (j in c(1:5)) {
    names <- c(names, paste(c("s", i, "l", j), collapse = ""))
  }
}

index<-c(1:25)

r2vector<-c(resultsA[1,])
plot(index,r2vector,type="n",main="Panel A", ylab = "R^2", xlab = "index")
text(index,r2vector,labels=names)

r2vector<-c(resultsB[1,])
plot(index,r2vector,type="n",main="Panel B", ylab = "R^2", xlab = "index")
text(index,r2vector,labels=names)

r2vector<-c(resultsC[1,])
plot(index,r2vector,type="n",main="Panel C", ylab = "R^2", xlab = "index")
text(index,r2vector,labels=names)

dev.off()






