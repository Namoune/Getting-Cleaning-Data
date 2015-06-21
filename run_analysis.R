# Getting and Cleaning Data Course Project
# R Script that does the following

# 1. Merges the training and the test sets to create one data set.

x_train <- read.table("train/X_train.txt")
x_test <- read.table("test/X_test.txt")
D1 <- rbind(x_train, x_test)

subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
S <- rbind(subject_train, subject_test)

y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
D2 <- rbind(y_train, y_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features_tbl <- read.table("features.txt")
ptr_select <- grep("-mean\\(\\)|-std\\(\\)", features_tbl[, 2])
D1 <- D1[, ptr_select]
names(D1) <- features_tbl[ptr_select, 2]
names(D1) <- gsub("\\(|\\)", "", names(D1))
names(D1) <- tolower(names(D1))

# 3. Uses descriptive activity names to name the activities in the data set.

activities_tbl <- read.table("activity_labels.txt")
activities_tbl[, 2] = gsub("_", "", tolower(as.character(activities_tbl[, 2])))
D2[,1] = activities_tbl[D2[,1], 2]
names(D2) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned_tbl <- cbind(S, D2, D1)
write.table(cleaned_tbl, "clean_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities_tbl[,1])
numCols = dim(cleaned_tbl)[2]
result = cleaned_tbl[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities_tbl[a, 2]
    X <- cleaned_tbl[cleaned_tbl$subject==s & cleaned_tbl$activity==activities_tbl[a, 2], ]
    result[row, 3:numCols] <- colMeans(X[, 3:numCols])
    row = row+1
  }
}
write.table(result, file = "tidydata.txt",row.name=FALSE)
