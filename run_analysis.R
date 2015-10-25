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
complete <- cbind(subject_all, y_all, x_all)

## Appropriately label the data set with descriptive variable names. 

featureNames <- as.character(features[,2])
newCols <- c("subject", "activity", featureNames)
colnames(complete) <- newCols

## Extract only the measurements on the mean and standard deviation for each measurement.
MEANS <- grep("mean()", colnames(complete))
STDEVS <- grep("std()", colnames(complete))
new_columns <- c(MEANS, STDEVS)
new_columns2 <- sort(new_columns)
new_tab <- complete[, c(1,2,new_columns2)]
clean_new_tab <- new_tab[, !grepl("Freq", colnames(new_tab))]

## Trim to show mean values for each subject-activity pair
tidy_data <- data.frame()
for (i in 1:30) {
      subject2<- subset(clean_new_tab,subject==i)
      for (j in 1:6){
            activity2<- subset(subject2, activity==j)
            pair<-as.vector(apply(activity2,2,mean))
            tidy_data<-rbind(tidy_data,pair) 
      }
      
}

## Use descriptive activity names to name the activities in the data set
## Create a second, independent tidy data set with the average of each 
## variable for each activity and each subject.

colnames(tidy_data)<-colnames(clean_new_tab)
levels(tidy_data[,2])<-c("Walking","Walking Upstairs", "Walking Downstairs", 
                         "Sitting", "Standing", "Laying")
write.table(tidy_data, "Tidy_Data.txt", sep = "", row.names = FALSE)

