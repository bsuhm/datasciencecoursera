dir <- "rprog-Assignment3"  # where data files live

val2float <- function(val) {
  # convert "Not Available" values from Medicare data to R NAs
  if (val=="Not Available") NA else as.numeric(val)
}

rank_mortality <- function(state, outcome) {
    # read outcome data and return a list in increasing order of mortality rates for <outcome> in <state>
  
    # Read outcome data 
    data <- read.csv(paste(dir,sep="/","outcome-of-care-measures.csv"),as.is=TRUE)
    if (outcome == 'heart attack' || outcome =='heart failure' || outcome == 'pneumonia') {
      sprintf("Read %d records",nrow(data))
    } else stop("invalid outcome")

    # Check that state and outcome are valid
    i<-1
    name<-c()
    mortality<-c()
    found<-FALSE   # data is not necessarily ordered by state, hence need to scan complete set and remember whether I found the state in question
    while (i<nrow(data)) {

      if (data[i,]$State==state) {
        # found data from the matching state
        if (outcome == 'pneumonia') {
          m <-val2float(data[i,]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        } else if (outcome == 'heart attack') {
          m <-val2float(data[i,]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        } else if (outcome == 'heart failure') {
          m <-val2float(data[i,]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        } 
        found<-TRUE
        name<-c(name,data[i,]$Hospital.Name)
        mortality<-c(mortality,m) 
      }
      i<-i+1
    }
    if (!found) stop("invalid state")

    # Return hospital name in that state with lowest 30-day death rate
    s<-data.frame(name,mortality)
#    attach(s)
    s_sorted<-s[order(s$mortality,s$name),]   # resolve ties by sorting by name as second

    s_sorted
}

best <- function (state,outcome) {
    mort_rank <- rank_mortality(state,outcome)
    mort_rank[1,]$name
}

rankhospital <- function (state, outcome, num = "best") {
    mort_rank <- rank_mortality(state,outcome)
    if (num=="best") {
      best(state,outcome)
    } else {
      N <- nrow(mort_rank)

      if (num == "worst") mort_rank[N,]$name
      else {
        if (N<num) stop(paste("invalid rank",num))
        mort_rank[N,]$name
      }
    }
}