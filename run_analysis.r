#Loading required package
library(dplyr)

#Download the dataset
filename <- "course_project.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Readind all data tables
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Cleaning data
#Requirement1: Merges the training and the test sets to create one data set.

x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
merged_data <- cbind(subject, y, x)

#Requirement2: Extracts only the measurements on the mean and standard deviation for each measurement.
cleandata <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

#Requirement3: Uses descriptive activity names to name the activities in the data set
cleandata$code <- activities[cleandata$code, 2]

#requirement4: Appropriately labels the data set with descriptive variable names.
names(cleandata)[2] = "activity"
names(cleandata)<-gsub("Acc", "Accelerometer", names(cleandata))
names(cleandata)<-gsub("Gyro", "Gyroscope", names(cleandata))
names(cleandata)<-gsub("BodyBody", "Body", names(cleandata))
names(cleandata)<-gsub("Mag", "Magnitude", names(cleandata))
names(cleandata)<-gsub("^t", "Time", names(cleandata))
names(cleandata)<-gsub("^f", "Frequency", names(cleandata))
names(cleandata)<-gsub("tBody", "TimeBody", names(cleandata))
names(cleandata)<-gsub("-mean()", "Mean", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("-std()", "STD", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("-freq()", "Frequency", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("angle", "Angle", names(cleandata))
names(cleandata)<-gsub("gravity", "Gravity", names(cleandata))

#Requirement 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finaldata <- cleandata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
  
write.table(finaldata, "FinalData.txt", row.name=FALSE)
