corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ids <- c()
  covs <- c()
  
  for(n in c(1:332)){
    filename <- paste("000", n, sep="")
    filename <- substr(filename, nchar(filename) - 2, nchar(filename))
    filename <- paste(directory, "/", filename, ".csv", sep="")
    #print(filename)
    x <- read.csv(filename, head=TRUE)
    y <- subset(x, !is.na(x$sulfate) & !is.na(x$nitrate))

    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    if(nrow(y) >= threshold){
      z <- cor(y$sulfate, y$nitrate)
      #z <- cor(x$sulfate, x$nitrate, na.rm=TRUE)
      #print(z)
      covs <- c(covs, z)
    }
  }
  
  ## Return a numeric vector of correlations
  covs
}
