library(dplyr)
library(plyr)
# set working directory
setwd("./Documents/06 programming/data cleaning")

# download train & test data
train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")

# Merges the training and the test sets to create one data set.
merged_data <- rbind(train_data, test_data)

# Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
mean_or_std_num <- grep("mean|std", features)
extracted_data <- merged_data[, mean_or_std_num]

## set subject column (subject, V1, V2....)
read.table("./UCI HAR Dataset/train/subject_train.txt") %>%
  unlist() -> train_subject
read.table("./UCI HAR Dataset/test/subject_test.txt")%>%
  unlist() -> test_subject
extracted_data <- cbind(c(train_subject, test_subject), extracted_data)
colnames(extracted_data)[1] <- "subject"

## set activity column(activity, subject, V1, V2....)
read.table("./UCI HAR Dataset/train/y_train.txt") %>%
  unlist() -> train_activity
read.table("./UCI HAR Dataset/test/y_test.txt") %>%
  unlist() -> test_activity
extracted_data <- cbind(c(train_activity, test_activity), extracted_data)
colnames(extracted_data)[1] <- "activity"

head(extracted_data)

# Uses descriptive activity names to name the activities in the data set(1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING)
WALKING <- arrange(filter(extracted_data, activity==1)[, -1], subject)
WALKING_UPSTAIRS <- arrange(filter(extracted_data, activity==2)[, -1], subject)
WALKING_DOWNSTAIRS <- arrange(filter(extracted_data, activity==3)[, -1], subject)
SITTING <- arrange(filter(extracted_data, activity==4)[, -1], subject)
STANDING <- arrange(filter(extracted_data, activity==5)[, -1], subject)
LAYING <- arrange(filter(extracted_data, activity==6)[, -1], subject)


# Appropriately labels the data set with descriptive variable names.
mean_or_std_labels <- grep("mean|std", features, value=TRUE)

mean_or_std_labels <- sub("\\()", "", mean_or_std_labels)
mean_or_std_labels <- gsub("\\(", "_", mean_or_std_labels)
mean_or_std_labels <- gsub("\\)", "", mean_or_std_labels)
mean_or_std_labels <- gsub(",|-", "_", mean_or_std_labels)
mean_or_std_labels <- sub("^t", "time", mean_or_std_labels)
mean_or_std_labels <- sub("^f", "frequency", mean_or_std_labels)
mean_or_std_labels <- sub("Acc", "Acceleration", mean_or_std_labels)

colnames(WALKING)[-1] <- mean_or_std_labels
colnames(WALKING_UPSTAIRS)[-1] <- mean_or_std_labels
colnames(WALKING_DOWNSTAIRS)[-1] <- mean_or_std_labels
colnames(SITTING)[-1] <- mean_or_std_labels
colnames(STANDING)[-1] <- mean_or_std_labels
colnames(LAYING)[-1] <- mean_or_std_labels

## labelsをXYZで更に区別すべき？

# creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## each activity

### WALKING
WALKING_AVERAGE <- WALKING[1,]
WALKING_AVERAGE
for(i in  3:80){
  WALKING_AVERAGE[, i] <- mean(unlist(select(WALKING, i)))
}
WALKING_AVERAGE <- WALKING_AVERAGE[, -1]

### WALKING_UPSTAIRS
WALKING_UPSTAIRS_AVERAGE <- WALKING_UPSTAIRS[1,]
for(i in  3:80){
  WALKING_UPSTAIRS_AVERAGE[, i] <- mean(unlist(select(WALKING_UPSTAIRS, i)))
}
WALKING_UPSTAIRS_AVERAGE <- WALKING_UPSTAIRS_AVERAGE[, -1]

### WALKING_DOWNSTAIRS
WALKING_DOWNSTAIRS_AVERAGE <- WALKING_DOWNSTAIRS[1,]
for(i in  3:80){
  WALKING_DOWNSTAIRS_AVERAGE[, i] <- mean(unlist(select(WALKING_DOWNSTAIRS, i)))
}
WALKING_DOWNSTAIRS_AVERAGE <- WALKING_DOWNSTAIRS_AVERAGE[, -1]

### SITTING
SITTING_AVERAGE <- SITTING[1,]
for(i in  3:80){
  SITTING_AVERAGE[, i] <- mean(unlist(select(SITTING, i)))
}
SITTING_AVERAGE <- SITTING_AVERAGE[, -1]

### STANDING
STANDING_AVERAGE <- STANDING[1,]
for(i in  3:80){
  STANDING_AVERAGE[, i] <- mean(unlist(select(STANDING, i)))
}
STANDING_AVERAGE <- STANDING_AVERAGE[, -1]

### LAYING
LAYING_AVERAGE <- LAYING[1,]
for(i in  3:80){
  LAYING_AVERAGE[, i] <- mean(unlist(select(LAYING, i)))
}
LAYING_AVERAGE <- LAYING_AVERAGE[, -1]

### ACTIVITY

rbind(WALKING_AVERAGE, WALKING_UPSTAIRS_AVERAGE) %>%
  rbind(WALKING_DOWNSTAIRS_AVERAGE) %>%
  rbind(SITTING_AVERAGE) %>%
  rbind(STANDING_AVERAGE) %>%
  rbind(LAYING_AVERAGE) -> ACTIVITY_AVERAGE
    
## each subject
SUBJECT_AVERAGE <- WALKING_AVERAGE[ ,-1]
for(k in 1:30){for(i in 3:81){mean(unlist(select(filter(extracted_data, subject == k),i))) -> SUBJECT_AVERAGE[k,i-2]}}
dim(SUBJECT_AVERAGE)

