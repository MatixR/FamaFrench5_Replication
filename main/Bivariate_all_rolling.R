################################################################################
### The following code performs a rolling regression on all bivariate sorts
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

# Data paths and names
datapaths <- paste0("../data/bivariate_sorts/", 
                    list.files("../data/bivariate_sorts"))
filenames <- gsub(".csv", "", list.files("../data/bivariate_sorts"), 
                  ignore.case = TRUE)
savepath  <- "../results/bivariate_sorts_rolling.pdf"

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

for (i in (1:length(datapaths))) {
  
  # temporary vars 
  path     <- datapaths[i]
  name     <- filenames[i]
  results  <- regressFactorsRolling(path, dataFactors, factors, 
                                    startDate, endDate, window)
  
  for (sort in c(1:25)) {
    
    title <- paste(c("5 Factor Rolling Regression on", (names(results))[sort],
                      "from the portfolio", name), 
                   collapse =" ")
    
    plot(results[[sort]], main = title, xlab = "date" )
    
  }
  
}

dev.off()


