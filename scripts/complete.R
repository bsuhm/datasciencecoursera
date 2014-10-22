complete <- function(directory, ids = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  output<-data.frame()
  i<-0
  id<-c()
  nobs<-c()
  
  for (f_id in ids) {
    # read next data file, building name of data file from directory and ID
    file<-paste(directory,"/",sprintf("%03d",f_id),".csv",sep="")
    data <- read.csv(file,TRUE,sep=",")
    i<i+1
 
    ok_sulf<-!is.na(data$sulfate)
    ok_nit<-!is.na(data$nitrate)
    
    id <- c(id,f_id)
    nobs <- c(nobs,sum(ok_sulf & ok_nit))
  }
  
  data.frame(id,nobs)
}