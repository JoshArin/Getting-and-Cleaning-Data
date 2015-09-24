# Getting-and-Cleaning-Data

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following. 
Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement. 
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names. 
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Solution to the question

filesPath <- "E:/Josh_Arin/Data Specialisation/Getting Data and Cleaning Data/Week 3/Course Project/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

## Unzipping DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Loading the required packages
library(dplyr)
library(data.table)
library(tidyr)

filesPath <- "E:/Josh_Arin/Data Specialisation/Getting Data and Cleaning Data/Week 3/Course Project/UCI HAR Dataset"
## Reading subject files, activity files and data files
dataSubTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

dataActTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## 1. Merging the training and the test datasets to create one Data Set
## for both Activity and Subject files this will merge the training and the test datasets by
## row-binding and re-name variables "subject" and "activityNum"

alldataSubject <- rbind(dataSubTrain, dataSubTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActTrain, dataActTest)
setnames(alldataActivity, "V1", "activityNum")

## Combining the Data training and test files
dataTable <- rbind(dataTrain, dataTest)

# Naming variables according to features for example, (V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

# Column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
AlldataSubjectActivity<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(AlldataSubjectActivity, dataTable)

### 2. Extracting only the measurements on the mean and standard deviation for each measurement
## Reading "features.txt" and extracting only the mean and standard deviation

dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

### 3. Uses descriptive activity names to name the activities in the data set

## Enter the name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## Creating dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

### 4. Appropriate labelling of the dataset with descriptive variable names 

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# Names after
head(str(dataTable),6)

### 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject

## writen to text file
write.table(dataTable, "TidyData.txt", row.name=FALSE)

