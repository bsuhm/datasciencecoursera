# cleaning script for HAR data set, course project for "Getting Cleaning Data" course offered by Johns Hopkins via Coursera
# Author: Bernhard Suhm
# Starting point: directory where we want to create  the subdirectory below for the HAR data 
data_dir <- "UCI HAR Dataset"

if (!file.exists(data_dir)) {
  # step A: download raw data, if we  haven't already
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url,destfile="./HAR_smartphones.zip")
  unzip("HAR_smartphones.zip")  
}
setwd("UCI HAR Dataset")

# setp B: read the actual data files. They consist of 561 variables (columns), the file is fixed width, each field 16 positions long
data_test <- read.table("test/X_test.txt",header=FALSE)
data_train <- read.table("train/X_train.txt",header=FALSE)

# step C (#2 in the instructions): pick out those columns that correspond to means or standard devisions.
# We determine which columns correspond to mean or std variables, based on their names. 
# We'll do that based on the inventory of variable names in features.txt
features <- read.csv("features.txt",sep=" ",header=FALSE)
colnames(features)<-c("Index","RawName")
# figure out which features represent means or stddev's
means<-grep("mean()",features$RawName)
# remove parens and dashes from variable names, because they'll cause trouble later when we want to build models with this data set
features$Name<-gsub("\\)","",gsub("\\(","",features$RawName))   
features$Name<-gsub("-","_",features$Name)

stds<-grep("std",features$Name) 
# select just those columns from data representing means or std, and give them the names as indicated by features.txt
test<-subset(data_test,select=c(means,stds))
colnames(test)<-features[c(means,stds),]$Name
train<-subset(data_train,select=c(means,stds))
colnames(train)<-features[c(means,stds),]$Name

# step D: now get the activity and subject labels, and use the descriptive activity labels (#4 and 3 in the instructions)
y_test<-read.csv("test/y_test.txt",sep=" ",header=FALSE)
y_train<-read.csv("train/y_train.txt",sep=" ",header=FALSE)
label<-c("Walking","Walking-Upstairs","Walking-Downstairs","Sitting","Standing","Laying")
l <- function(y) { if(y>0 & y<7) {label[y]} }
activities<-mutate(y_test,Y=l(V1))
test<-mutate(test,Activity=activities$Y)   # adding Activity column to test data frame
activities<-mutate(y_train,Y=l(V1))
train<-mutate(train,Activity=activities$Y)   # adding Activity to train data frame

subject_test<-read.csv("test/subject_test.txt",sep=" ",header=FALSE)
colnames(subject_test)<-c("Subject")
test<-cbind(subject_test,test)
subject_train<-read.csv("train/subject_train.txt",sep=" ",header=FALSE)
colnames(subject_train)<-c("Subject")
train<-cbind(subject_train,train)

# now we have both test and train the desired cleaned-up shape, let's merge them (#1 in the instructions)
all_data <- rbind(test,train)

# step E (or #5): group by Activity and Subject, and compute means
agg_data <- aggregate(all_data,by=list(all_data$Subject,all_data$Activity),FUN=mean)
# remove duplicated Subject column and messed up Activity columns
tmp<-subset(agg_data,select=c(-1,-83))
data <- rename(tmp,Activity=Group.2)
write.table(data,"data_tidy.txt",sep=" ",row.name=FALSE)

