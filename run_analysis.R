"The purpose of this project is to demonstrate ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. Required to submit: 
1) a tidy data set as described below, 
2) a link to a Github repository with script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that performed 
  to clean up the data called CodeBook.md.


Assignment

Getting and cleaning data - Course work

Student have to create one R script called run_analysis.R that does the following:
 Merges the training and the test sets to create one data set.
 Extracts only the measurements on the mean and standard deviation for each measurement.
 Uses descriptive activity names to name the activities in the data set.
 Appropriately labels the data set with descriptive variable names.
 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
"

# Prerequisites

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

if (!is.installed("data.table")){
    install.packages("data.table")
}

if (!is.installed("plyr")){
    install.packages("plyr")
}

library("data.table")
library("plyr")

# 0. Download and extract dataset

    urlFile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFileName = "Dataset.zip"
    DataDir="."

## Download File
if (!file.exists(zipFileName)){
    download.file(url=urlFile, destfile=zipFile)
}

## unzip file
    unzip(zipfile=zipFileName, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = DataDir, unzip = "internal",
          setTimes = FALSE)

# 1. Merges the training and the test sets to create one data set.

# Reading files

# Reading trainings tables:
    x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
    X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Reading feature vector:
    features <- read.table('./UCI HAR Dataset/features.txt')

# Reading activity labels:
    activity_labels = read.table('./UCI HAR Dataset/activity_labels.txt')

# Assigning column names:
    colnames(x_train) <- features[,2] 
    colnames(y_train) <-"activityId"
    colnames(subject_train) <- "subjectId"

    colnames(X_test) <- features[,2] 
    colnames(y_test) <- "activityId"
    colnames(subject_test) <- "subjectId"

    colnames(activity_labels) <- c('activityId','activityType')

# 1.3 Merging all data in one set:
    merge_train <- cbind(y_train, subject_train, x_train)
    merge_test <- cbind(y_test, subject_test, X_test)
    setAllInOne <- rbind(merge_train, merge_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Reading column names:
    colnames <- colnames(setAllInOne)

# Create vector for defining ID, mean and standard deviation:
    mean_and_std <- (grepl("activityId" , colnames) | 
                   grepl("subjectId" , colnames) | 
                   grepl("mean.." , colnames) | 
                   grepl("std.." , colnames) 
)

# subset from setAllInOne:
    setFor_Mean_And_Std <- setAllInOne[ , mean_and_std == TRUE]

# 3. Uses descriptive activity names to name the activities in the data set.
    seActivityNames <- merge(setFor_Mean_And_Std, activity_labels,
                              by='activityId',
                              all.x=TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
# above


# 5. From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.

# Second tidy data set 
    secTidySet <- aggregate(. ~subjectId + activityId, seActivityNames, mean)
    secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

# Second tidy data set in txt file
    write.table(secTidySet, "secTidySet.txt", row.name=FALSE)