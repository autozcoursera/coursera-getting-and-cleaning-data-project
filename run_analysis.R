# run_analysis.R file description:
#
# This script of purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#
# here use data : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# data files including:
# features.txt
# activity_labels.txt
# x_train.txt
# x_test.txt
# y_train.txt
# y_test.txt
# subject_train.txt
# subject_test.txt
# 
# with following steps
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names.
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Step 0. Set Working Directory, download, read and upzip the data
setwd("D:/gitcode/Rlan")
filename <- "FUCI_dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename)
}
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}
## Step 1. Merges the training and the test sets to create one data set.
# Read features and activity_labels data
features    <-  read.table('./UCI HAR Dataset/features.txt',header=FALSE); 
activity_labels <-  read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);
# Read features data and set titles
x_train     <-  read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE); 
x_test      <-  read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE); 
x_data      <- rbind(x_train,x_test)
colnames(x_data) <- features$V2
# Read activities data and set titles
y_train     <-  read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);
y_test      <-  read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); 
y_data      <- rbind(y_train,y_test)
colnames(y_data) <- "activityId"
# Read subjects data and set titles
subject_train <-  read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
subject_test <-  read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
subject_data <- rbind(y_train,y_test)
colnames(subject_data) <- "subjectId"
# merge the final data and remove temporary variable
final_data <- cbind(subject_data,y_data,x_data)
rm(list = ls(pattern = "test"))
rm(list = ls(pattern = "train"))

## Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
columns <- names(final_data)[grep("subject|activity|mean|std", colnames(final_data))]
final_data <- final_data[columns]

## Step 3. Uses descriptive activity names to name the activities in the data set
names(activity_labels) <- c("activityId", "activityName")
final_data <- merge(activity_labels,final_data,"activityId")
final_data <- select(final_data, -activityId)

#Step 4 Appropriately labels the data set with descriptive variable names.
# get column names
final_data_col <- colnames(final_data)
# set labels with details descriptive variable names 
final_data_col <- gsub("[\\(\\)-]", "", final_data_col)
final_data_col <- gsub("^f", "frequency", final_data_col)
final_data_col <- gsub("^t", "time", final_data_col)
final_data_col <- gsub("Acc", "Accelerometer", final_data_col)
final_data_col <- gsub("Gyro", "Gyroscope", final_data_col)
final_data_col <- gsub("Mag", "Magnitude", final_data_col)
final_data_col <- gsub("Freq", "Frequency", final_data_col)
final_data_col <- gsub("mean", "Mean", final_data_col)
final_data_col <- gsub("std", "StandardDeviation", final_data_col)
final_data_col <- gsub("BodyBody", "Body", final_data_col)
# rename with new labels
colnames(final_data) <- final_data_col

## Step 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# group by subject and activity and summarise using mean
new_data_means <- final_data %>% 
group_by(activityName, subjectId) %>%
summarise_all(mean)
new_data_means <- arrange(new_data_means,subjectId)

# output the data
write.table(new_data_means, "new_data_means.txt", row.names = FALSE, quote = FALSE)
