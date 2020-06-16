# Getting and Cleaning Data Project Assignment

# Criteria
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# load packages and get data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip"))
unzip(zipfile = "data.zip")

# load activity labels and features
activity_labels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "feature_names"))
features_wanted <- grep("(mean|std)\\(\\)", features[, feature_names])
measurements <- features[features_wanted, feature_names]
measurements <- gsub('[()]', '', measurements)

# load train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, features_wanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
train_activities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
train_subjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(train_subjects, train_activities, train)

# load test datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, features_wanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
test_activities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
test_subjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(test_subjects, test_activities, test)

# merge datasets and add labels
combined <- rbind(train, test)

# convert classLabels to activityName. 
combined[["Activity"]] <- factor(combined[, Activity]
                                 , levels = activity_labels[["classLabels"]]
                                 , labels = activity_labels[["activityName"]])

combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
combined <- reshape2::melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- reshape2::dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

# create final tidy_data in csv file.
data.table::fwrite(x = combined, file = "tidy_data.csv", quote = FALSE)