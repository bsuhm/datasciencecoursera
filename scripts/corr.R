read_complete <- function(file) {
  ## reads .csv <file> that's expected to contain pollution monitoring data, i.e.
  ## cases of sulfate and nitrate measurements
  ##
  ## returns the subset of complete data points as list
  
  d <- read.csv(file,TRUE,sep=",") 
  ok<-!is.na(d$sulfate) & !is.na(d$nitrate)
  
  list(n=sum(ok),sulfate=d$sulfate[ok],nitrate=d$nitrate[ok])
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  cor_v <- c()

  # read file in directory
  
  for (item in dir(directory)) {
    # read next data file, building name of data file from directory and ID
    file<-paste(directory,"/",item,sep="")
    data <- read_complete(file)

    if (data$n>threshold) {
      x<-data$nitrate
      y<-data$sulfate
 
      cor_v <- c(cor_v,cor(x,y))
    }
  }
  cor_v
}

