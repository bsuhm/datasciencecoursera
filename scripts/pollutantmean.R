pollutantmean <- function(directory, pollutant, ids = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## verbose provides a little feedback about progress to stdout
  ##
  ## EXAMPLE CALL: pollutantmean(datadir,"nitrate",c("001","002","003"))
  
  # check validity of pollutant argument
  if (pollutant!="sulfate" & pollutant!="nitrate") { 
    if (verbose) {print("WARNING: invalid pollutant requested")}
    return
  }
  
  # initialize total sum of values and count of "good" (not NA) data points
  total_sum <- 0
  total_good <- 0
  
  # now loop over all input files, identified by index <id>
  if (verbose) {print("Total good data points read: ")}
  for (id in ids) {
    # read next data file, building name of data file from directory and ID
    file<-paste(directory,"/",sprintf("%03d",id),".csv",sep="")
    data <- read.csv(file,TRUE,sep=",")
    
    if (pollutant=="sulfate") {
      pol_data<-data$sulfate
    } else {
      pol_data<-data$nitrate
    } 

    # extract good data (eliminating missing data)
    ok_data<-!is.na(pol_data)
    total_good <- total_good + sum(ok_data)
    total_sum <- total_sum + sum(pol_data[ok_data])

    if (verbose) {print(total_good)}
  }
  
  return(total_sum/total_good)
}
