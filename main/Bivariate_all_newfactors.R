################################################################################
### The following code performs a linear regression on all bivariate sorts
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
  results <- data.frame(row.names = c("adj r^2","intercept", factors))
  
  for (i in colnames(dataPortfolio[2:ncol(dataPortfolio)])) {
    
    # OLS on excess returns
    fit <- lm(as.numeric(unlist(dataPortfolio[i]-dataFactors["RF"])) ~ ., 
              data = dataFactors[factors])
    
    # Add regression to results
    results[i] <- c(summary(fit)$adj.r.squared, 
                    summary(fit)$coefficients[1:(length(factors)+1)])  
    
  }
  
  results
  
}


## Factor Data

# Get 5 Factor Returns Data
datapathF   <- "../data/returns/QMJ_HMLD.csv"
dataFactors <- read.csv(file=datapathF, header=TRUE, sep=",")


## Main

# Relevant dates
startDate <- as.Date("July 01 1963", format = "%B %d %Y")
endDate   <- as.Date("December 01 2013", format = "%B %d %Y")

# Relevant factors
factors1  <- c("Mkt.RF", "SMB", "HML", "RMW", "CMA")
factors2  <- c("Mkt.RF", "SMB", "HMLD", "RMW", "CMA")

# Data paths and names
datapaths <- paste0("../data/bivariate_sorts/", 
                    list.files("../data/bivariate_sorts"))
filenames <- gsub(".csv", "", list.files("../data/bivariate_sorts"), 
                  ignore.case = TRUE)
savepath  <- "../results/bivariate_sorts_newfactors.pdf"

# Plot labels
labels <- c()
for (i in c(1:5)) {
  for (j in c(1:5)) {
    labels <- c(labels, paste(c("x", i, "y", j), collapse = ""))
  }
}
index <- c(1:25)

# Run regressions and save to pdf
pdf(savepath)

for (i in c(1:length(datapaths))) {
  
  # temporary vars 
  path     <- datapaths[i]
  name     <- filenames[i]
  title    <- paste("Adjusted R^2 on", name)
  
  
  # Get results
  results1 <- regressFactors(path, dataFactors, factors1, startDate, endDate)
  results2 <- regressFactors(path, dataFactors, factors2, startDate, endDate)
  
  r2vector1 <-c(results1[1,])
  r2vector2 <-c(results2[1,])
  
  plot(index, r2vector1, type="n", main= title, ylab = "R^2", xlab = "index")
  text(index, r2vector1, labels = labels, col = "red")
  points(index, r2vector2, col = "white")
  text(index, r2vector2, labels = labels, col = "blue")
  legend("bottomleft",
         legend = c(paste(factors1, collapse = "+"), 
                    paste(factors2, collapse = "+")),
         col = c("red", "blue"), pch = 1, cex = 0.7)
  
}

dev.off()


