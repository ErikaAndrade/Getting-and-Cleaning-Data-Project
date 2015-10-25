## Getting and Cleaning Data - Class Project

## Download and extract data from:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Read the data files into R
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

## Merge the training and the test sets to create one data set.
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)
complete <- cbind(subject_all, x_all, y_all)

## Appropriately label the data set with descriptive variable names. 
featureNames <- data.frame()
newCols <- data.frame()
clean_new_tab <- data.frame()
MEANS <- data.frame()
STDEVS <-data.frame()
new_columns <- data.frame()


featureNames <- as.character(features[,2])
newCols <- c("subject", "activity", featureNames)
colnames(complete) <- newCols

## Extract only the measurements on the mean and standard deviation for each measurement.
MEANS <- grep("mean()", colnames(complete))
STDEVS <- grep("std()", colnames(complete))
new_columns <- sort(c(MEANS, STDEVS))
new_tab <- complete[, c(1,2,new_columns)]
clean_new_tab <- new_tab[, !grepl("Freq", colnames(new_tab))] #get rid of the meanFreq columns

## Trim the rows to the 180 needed to show mean values for each subject-activity pair
tidyframe <- data.frame()
for (i in 1:30) {
      subj<- subset(clean_new_tab,subject==i)
      for (j in 1:6){
            actv<- subset(subj, activity==j)
            myresult<-as.vector(apply(actv,2,mean))
            tidyframe<-rbind(tidyframe,myresult) 
      }
      
}

## Use descriptive activity names to name the activities in the data set
## Create a second, independent tidy data set with the average of each 
## variable for each activity and each subject.

colnames(tidyframe)<-colnames(clean_new_tab) #rename the columns again, as the names get lost in the mix above
levels(tidyframe[,2])<-c("Walking","Walking Upstairs", "Walking Downstairs", 
                         "Sitting", "Standing", "Laying")
write.table(tidyframe, "Tidy_Data.txt", sep = "")

