## CODE_BOOK

# Describing the Data
The data used was obtained from experiments which were carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 

Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz were captured. 
The experiments were video-recorded to label the data manually. The obtained dataset was randomly partitioned into two sets, 
where 70% of the volunteers was selected for generating the training data and 30% the test data.

The features selected for this database came from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
The acceleration signal was separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) – both using a low pass Butterworth filter.

The body linear acceleration and angular velocity were derived to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).
A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).

Description of abbreviations of measurements

leading t & f are based on time or frequency measurements respectively.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = accelerometer measurement
Gyro = gyroscopic measurements
Jerk = sudden movement acceleration
Mag = magnitude of movement
mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. A total of 33 measurements including the 3 dimensions - the X,Y, and Z axes.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag 
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables to be estimated from these signals are:
mean(): Mean (average value)
std(): Standard deviation

### Downloading the data on a Windows machine
filesPath <- "E:/Josh_Arin/Data Specialisation/Getting Data and Cleaning Data/Week 3/Course Project/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

### Unzipping the dataSet
unzip(zipfile="./data/Dataset.zip",exdir="./data")

### Loading the necessary packages. NB: The packages are installed prior to this step
library(dplyr)
library(data.table)
library(tidyr)

Files in folder ‘UCI HAR Dataset’ that will be used are:

SUBJECT FILES
test/subject_test.txt
train/subject_train.txt
ACTIVITY FILES
test/X_test.txt
train/X_train.txt
DATA FILES
test/y_test.txt
train/y_train.txt
features.txt - Names of column variables in the dataTable
activity_labels.txt - Links the class labels with their activity name.

### Reading the above files and creating data tables
# Reading subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Reading activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Reading data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

1. Mergeing the "training" and the "test" sets to create one data set
# The training and the test sets are going to be merged by row binding in for both activity and subject and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

# Combining the data training and test files
dataTable <- rbind(dataTrain, dataTest)

# Naming variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

# Column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merging columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

2. Extracting only the measurements on the mean and standard deviation for each measurement
# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

3. Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## Creating dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

4. Appropriate labeling of the data set with descriptive variable names
leading t or f is based on time or frequency measurements.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = accelerometer measurement
Gyro = gyroscopic measurements
Jerk = sudden movement acceleration
Mag = magnitude of movement

mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

#Names before
head(str(dataTable),2)
## Classes 'tbl_df', 'tbl' and 'data.frame':    180 obs. of  69 variables:
##  $ subject                    : int  1 1 1 1 1 1 2 2 2 2 ...
##  $ activityName               : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ activityNum                : num  6 4 5 1 3 2 6 4 5 1 ...
##  $ tBodyAcc-mean()-X          : num  0.222 0.261 0.279 0.277 0.289 ...
##  $ tBodyAcc-mean()-Y          : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
##  $ tBodyAcc-mean()-Z          : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
##  $ tBodyAcc-std()-X           : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
##  $ tBodyAcc-std()-Y           : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
##  $ tBodyAcc-std()-Z           : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
##  $ tGravityAcc-mean()-X       : num  -0.249 0.832 0.943 0.935 0.932 ...
##  $ tGravityAcc-mean()-Y       : num  0.706 0.204 -0.273 -0.282 -0.267 ...
##  $ tGravityAcc-mean()-Z       : num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...
##  $ tGravityAcc-std()-X        : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...
##  $ tGravityAcc-std()-Y        : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...
##  $ tGravityAcc-std()-Z        : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...
##  $ tBodyAccJerk-mean()-X      : num  0.0811 0.0775 0.0754 0.074 0.0542 ...
##  $ tBodyAccJerk-mean()-Y      : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...
##  $ tBodyAccJerk-mean()-Z      : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
##  $ tBodyAccJerk-std()-X       : num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
##  $ tBodyAccJerk-std()-Y       : num  -0.924 -0.981 -0.986 0.067 -0.102 ...
##  $ tBodyAccJerk-std()-Z       : num  -0.955 -0.988 -0.992 -0.503 -0.346 ...
##  $ tBodyGyro-mean()-X         : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
##  $ tBodyGyro-mean()-Y         : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
##  $ tBodyGyro-mean()-Z         : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
##  $ tBodyGyro-std()-X          : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
##  $ tBodyGyro-std()-Y          : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
##  $ tBodyGyro-std()-Z          : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
##  $ tBodyGyroJerk-mean()-X     : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
##  $ tBodyGyroJerk-mean()-Y     : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
##  $ tBodyGyroJerk-mean()-Z     : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
##  $ tBodyGyroJerk-std()-X      : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...
##  $ tBodyGyroJerk-std()-Y      : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...
##  $ tBodyGyroJerk-std()-Z      : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...
##  $ tBodyAccMag-mean()         : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
##  $ tBodyAccMag-std()          : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
##  $ tGravityAccMag-mean()      : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
##  $ tGravityAccMag-std()       : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
##  $ tBodyAccJerkMag-mean()     : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
##  $ tBodyAccJerkMag-std()      : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
##  $ tBodyGyroMag-mean()        : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
##  $ tBodyGyroMag-std()         : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...
##  $ tBodyGyroJerkMag-mean()    : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...
##  $ tBodyGyroJerkMag-std()     : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...
##  $ fBodyAcc-mean()-X          : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
##  $ fBodyAcc-mean()-Y          : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
##  $ fBodyAcc-mean()-Z          : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...
##  $ fBodyAcc-std()-X           : num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
##  $ fBodyAcc-std()-Y           : num  -0.834 -0.917 -0.972 0.056 -0.113 ...
##  $ fBodyAcc-std()-Z           : num  -0.813 -0.934 -0.978 -0.28 -0.298 ...
##  $ fBodyAccJerk-mean()-X      : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
##  $ fBodyAccJerk-mean()-Y      : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
##  $ fBodyAccJerk-mean()-Z      : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...
##  $ fBodyAccJerk-std()-X       : num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
##  $ fBodyAccJerk-std()-Y       : num  -0.932 -0.983 -0.987 0.107 -0.135 ...
##  $ fBodyAccJerk-std()-Z       : num  -0.961 -0.988 -0.992 -0.535 -0.402 ...
##  $ fBodyGyro-mean()-X         : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...
##  $ fBodyGyro-mean()-Y         : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
##  $ fBodyGyro-mean()-Z         : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
##  $ fBodyGyro-std()-X          : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...
##  $ fBodyGyro-std()-Y          : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
##  $ fBodyGyro-std()-Z          : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...
##  $ fBodyAccMag-mean()         : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
##  $ fBodyAccMag-std()          : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...
##  $ fBodyBodyAccJerkMag-mean() : num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
##  $ fBodyBodyAccJerkMag-std()  : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...
##  $ fBodyBodyGyroMag-mean()    : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...
##  $ fBodyBodyGyroMag-std()     : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...
##  $ fBodyBodyGyroJerkMag-mean(): num  -0.942 -0.99 -0.995 -0.319 -0.282 ...
##  $ fBodyBodyGyroJerkMag-std() : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...
## NULL

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# Names after
head(str(dataTable),6)
## Classes 'tbl_df', 'tbl' and 'data.frame':    180 obs. of  69 variables:
##  $ subject                                       : int  1 1 1 1 1 1 2 2 2 2 ...
##  $ activityName                                  : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ activityNum                                   : num  6 4 5 1 3 2 6 4 5 1 ...
##  $ timeBodyAccelerometer-MEAN()-X                : num  0.222 0.261 0.279 0.277 0.289 ...
##  $ timeBodyAccelerometer-MEAN()-Y                : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
##  $ timeBodyAccelerometer-MEAN()-Z                : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
##  $ timeBodyAccelerometer-SD()-X                  : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
##  $ timeBodyAccelerometer-SD()-Y                  : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
##  $ timeBodyAccelerometer-SD()-Z                  : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
##  $ timeGravityAccelerometer-MEAN()-X             : num  -0.249 0.832 0.943 0.935 0.932 ...
##  $ timeGravityAccelerometer-MEAN()-Y             : num  0.706 0.204 -0.273 -0.282 -0.267 ...
##  $ timeGravityAccelerometer-MEAN()-Z             : num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...
##  $ timeGravityAccelerometer-SD()-X               : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...
##  $ timeGravityAccelerometer-SD()-Y               : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...
##  $ timeGravityAccelerometer-SD()-Z               : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...
##  $ timeBodyAccelerometerJerk-MEAN()-X            : num  0.0811 0.0775 0.0754 0.074 0.0542 ...
##  $ timeBodyAccelerometerJerk-MEAN()-Y            : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...
##  $ timeBodyAccelerometerJerk-MEAN()-Z            : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
##  $ timeBodyAccelerometerJerk-SD()-X              : num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
##  $ timeBodyAccelerometerJerk-SD()-Y              : num  -0.924 -0.981 -0.986 0.067 -0.102 ...
##  $ timeBodyAccelerometerJerk-SD()-Z              : num  -0.955 -0.988 -0.992 -0.503 -0.346 ...
##  $ timeBodyGyroscope-MEAN()-X                    : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
##  $ timeBodyGyroscope-MEAN()-Y                    : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
##  $ timeBodyGyroscope-MEAN()-Z                    : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
##  $ timeBodyGyroscope-SD()-X                      : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
##  $ timeBodyGyroscope-SD()-Y                      : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
##  $ timeBodyGyroscope-SD()-Z                      : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
##  $ timeBodyGyroscopeJerk-MEAN()-X                : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
##  $ timeBodyGyroscopeJerk-MEAN()-Y                : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
##  $ timeBodyGyroscopeJerk-MEAN()-Z                : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
##  $ timeBodyGyroscopeJerk-SD()-X                  : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...
##  $ timeBodyGyroscopeJerk-SD()-Y                  : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...
##  $ timeBodyGyroscopeJerk-SD()-Z                  : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...
##  $ timeBodyAccelerometerMagnitude-MEAN()         : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
##  $ timeBodyAccelerometerMagnitude-SD()           : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
##  $ timeGravityAccelerometerMagnitude-MEAN()      : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
##  $ timeGravityAccelerometerMagnitude-SD()        : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
##  $ timeBodyAccelerometerJerkMagnitude-MEAN()     : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
##  $ timeBodyAccelerometerJerkMagnitude-SD()       : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
##  $ timeBodyGyroscopeMagnitude-MEAN()             : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
##  $ timeBodyGyroscopeMagnitude-SD()               : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...
##  $ timeBodyGyroscopeJerkMagnitude-MEAN()         : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...
##  $ timeBodyGyroscopeJerkMagnitude-SD()           : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...
##  $ frequencyBodyAccelerometer-MEAN()-X           : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
##  $ frequencyBodyAccelerometer-MEAN()-Y           : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
##  $ frequencyBodyAccelerometer-MEAN()-Z           : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...
##  $ frequencyBodyAccelerometer-SD()-X             : num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
##  $ frequencyBodyAccelerometer-SD()-Y             : num  -0.834 -0.917 -0.972 0.056 -0.113 ...
##  $ frequencyBodyAccelerometer-SD()-Z             : num  -0.813 -0.934 -0.978 -0.28 -0.298 ...
##  $ frequencyBodyAccelerometerJerk-MEAN()-X       : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
##  $ frequencyBodyAccelerometerJerk-MEAN()-Y       : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
##  $ frequencyBodyAccelerometerJerk-MEAN()-Z       : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...
##  $ frequencyBodyAccelerometerJerk-SD()-X         : num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
##  $ frequencyBodyAccelerometerJerk-SD()-Y         : num  -0.932 -0.983 -0.987 0.107 -0.135 ...
##  $ frequencyBodyAccelerometerJerk-SD()-Z         : num  -0.961 -0.988 -0.992 -0.535 -0.402 ...
##  $ frequencyBodyGyroscope-MEAN()-X               : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...
##  $ frequencyBodyGyroscope-MEAN()-Y               : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
##  $ frequencyBodyGyroscope-MEAN()-Z               : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
##  $ frequencyBodyGyroscope-SD()-X                 : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...
##  $ frequencyBodyGyroscope-SD()-Y                 : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
##  $ frequencyBodyGyroscope-SD()-Z                 : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...
##  $ frequencyBodyAccelerometerMagnitude-MEAN()    : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
##  $ frequencyBodyAccelerometerMagnitude-SD()      : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...
##  $ frequencyBodyAccelerometerJerkMagnitude-MEAN(): num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
##  $ frequencyBodyAccelerometerJerkMagnitude-SD()  : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...
##  $ frequencyBodyGyroscopeMagnitude-MEAN()        : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...
##  $ frequencyBodyGyroscopeMagnitude-SD()          : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...
##  $ frequencyBodyGyroscopeJerkMagnitude-MEAN()    : num  -0.942 -0.99 -0.995 -0.319 -0.282 ...
##  $ frequencyBodyGyroscopeJerkMagnitude-SD()      : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...
## NULL

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)

Withing the tidy dataset, 10299 instances are split into 180 groups (30 subjects and 6 activities) and 66 mean and standard deviation features are averaged for each group. 
The resulting data table has 180 rows and 69 columns – 33 Mean variables + 33 Standard deviation variables + 1 Subject( 1 of of the 30 test subjects) + ActivityName + ActivityNum

The tidy data set’s first row is the header containing the names for each column.