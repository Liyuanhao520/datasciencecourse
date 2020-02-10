#reading data
library(dplyr)
library(reshape2)

#reading train data
X_train <- read.table(file = "a/train/X_train.txt")
y_train <- read.table("a/train/y_train.txt") 
trainingSubjects <- read.table("a/train/subject_train.txt")

#reading test data
X_test <- read.table("a/test/X_test.txt")
y_test <- read.table("a/test/y_test.txt") 
testSubjects <- read.table("a/test/subject_test.txt")

# read features, don't convert text labels to factors
features <- read.table(file = "a/features.txt",header = T)
features[,2] <- as.character(features[,2])

# read activity labels
activityLabels <- read.table(file = "a/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])

colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the training and the test sets to create one data set
train <- cbind(trainingSubjects, X_train, y_train)
test <- cbind(testSubjects, X_test, y_test)
humanActivity <- rbind(train,test)
rm(trainingSubjects, X_train, y_train, 
   testSubjects, X_test, y_test)
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# Step 3 - Use descriptive activity names to name the activities in the data set
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names
humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
colnames(humanActivity) <- humanActivityCols

# Step 5 - Create a second, independent tidy set with the average of each variable for each activity and each subject
humanActivity$activity <- factor(humanActivity$activity, levels = activityLabels[,1], labels = activityLabels[,2])
humanActivity$subject <- as.factor(humanActivity$subject)
Data.melted <- melt(humanActivity, id = c("subject", "activity"))
Data.mean <- dcast(Data.melted, subject + activity ~ variable, mean)
write.table(Data.mean, "tidy.txt", row.names = FALSE, quote = FALSE)


