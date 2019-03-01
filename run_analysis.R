library(dplyr) # in order to use %>%

# download zip file
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, destfile = "./data/Dataset.zip", method = "curl")

# unzip
unzip("./data/Dataset.zip", exdir ="./data")

# read features and convert easy character
features <- read.table("./data/UCI HAR Dataset/features.txt")[,2]
features <- gsub("^t", "time", features)  # t => time
features <- gsub("^f", "frequency", features)  # f => frequency

# read activity labels
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("activityID", "activity_name"))

# read train data set
X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
colnames(X_train) <- features
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "activityID")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# read test data set
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
colnames(X_test) <- features
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "activityID")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# Merges the training and the test sets to create one data set.
merged_train <- cbind(y_train, subject_train, X_train)
merged_test <- cbind(y_test, subject_test, X_test)
merged_all <- rbind(merged_train, merged_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.
mean_or_std_col <- grep("mean|std", features, value=TRUE)
extracted <- merged_all[, c("activityID", "subject", mean_or_std_col)]

# Uses descriptive activity names to name the activities in the data set.
extracted_with_names <- merge(extracted, activity_labels, all.x=TRUE)

# creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  aggregate(.~activityID+subject, data=extracted_with_names[, -82], FUN=mean) %>% #if include activity_name[column No. 82], the activity_name becomes number(not character).
  merge(activity_labels, all.x=TRUE) %>% # reassign activity_name
  write.table(file = "tidy_data_set.txt", row.names = FALSE)
