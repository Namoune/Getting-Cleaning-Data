# Part 1 - Merge the training and the test sets to create one data set

x_train_tbl <- read.table("train/X_train.txt")
x_test_tbl <- read.table("test/X_test.txt")
v1 <- rbind(x_train_tbl, x_test_tbl)

x_train_tbl <- read.table("train/subject_train.txt")
x_test_tbl <- read.table("test/subject_test.txt")
v2 <- rbind(x_train_tbl, x_test_tbl)

x_train_tbl <- read.table("train/y_train.txt")
x_test_tbl <- read.table("test/y_test.txt")
v3 <- rbind(x_train_tbl, x_test_tbl)

# Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement

features_tbl <- read.table("features.txt")
ptr_sel <- grep("-mean\\(\\)|-std\\(\\)", features_tbl[, 2])
v1 <- v1[, ptr_sel]
names(v1) <- features_tbl[ptr_sel, 2]
names(v1) <- gsub("\\(|\\)", "", names(v1))
names(v1) <- tolower(names(v1))

# Part 3 - Uses descriptive activity names to name the activities in the data set

activities_tbl <- read.table("activity_labels.txt")
activities_tbl[, 2] = gsub("_", "", tolower(as.character(activities_tbl[, 2])))
v2[,1] = activities_tbl[v2[,1], 2]
names(v2) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(v3) <- "subject"
cleaned_data <- cbind(v3, v2, v1)
write.table(cleaned_data, "cleaned.txt")

# Part 5 - From the data set in step 4, 
# creates a second, independent tidy data set with the average of each variable for each activity and each subject

idSubjects = length(unique(v3)[,1])
idActivities = length(activities_tbl[,1])
idCols = dim(cleaned_data)[2]
uniqueSubjects = unique(v3)[,1]
tidy_data = cleaned_data[1:(idSubjects*idActivities), ]

row = 1
for (v3 in 1:idSubjects) {
  for (x in 1:idActivities) {
    tidy_data[row, 1] = uniqueSubjects[v3]
    tidy_data[row, 2] = activities_tbl[x, 2]
    temporary <- cleaned_data[cleaned_data$subject==v3 & cleaned_data$activity==activities_tbl[x, 2], ]
    tidy_data[row, 3:idCols] <- colMeans(temporary[, 3:idCols])
    row = row+1
  }
}
write.table(tidy_data, "tidy.txt")
