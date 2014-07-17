#
# Creates a data set based upon the "UCI HAR Dataset" at https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
# The resulting data set contains all features from the training and test data sets, combined into a single data set.
# Activity labels and subjects are included for each row.
#
loadData <- function(nrows = 9999) {
    # Read feature labels.
    labels <- unlist(read.table("UCI HAR Dataset/features.txt", header = FALSE, colClasses = c("NULL", NA), stringsAsFactors = FALSE))
    
    # Clean labels, remove invalid characters.
    labels <- gsub("[^a-zA-Z0-9]", "", labels)
    labels <- gsub("mean", "Mean", labels)
    labels <- gsub("std", "Std", labels)
    
    # Read activity labels.
    activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("activityCode", "activityLabel"), stringsAsFactors = FALSE)
    
    # Read training features (561 total features).
    train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, nrows = nrows, col.names = labels, na.strings = "", stringsAsFactors = FALSE)
    
    # Read training activities.
    trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, nrows = nrows, col.names = c("activityCode"), na.strings = "", stringsAsFactors = FALSE)
    
    # Append activity labels next to their associated code.
    trainActivities$activityLabel <- activityLabels$activityLabel[match(trainActivities$activityCode, activityLabels$activityCode)]
    
    # Append activities to train data. Note, to include the activity code, uncomment the cbind line and comment out the line below it.
    #train <- cbind(train, trainActivities)
    train$activity <- trainActivities$activityLabel
    
    # Read train subjects.
    subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, nrows = nrows, col.names = c("subject"), na.strings = "", stringsAsFactors = FALSE)

    # Append subjects to train data.
    train <- cbind(train, subjects)
    
    # Read test features (561 total features).
    test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, nrows = nrows, col.names = labels, na.strings = "", stringsAsFactors = FALSE)
    
    # Read test activities.
    testActivities <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, nrows = nrows, col.names = c("activityCode"), na.strings = "", stringsAsFactors = FALSE)
    
    # Append activity labels next to their associated code.
    testActivities$activityLabel <- activityLabels$activityLabel[match(testActivities$activityCode, activityLabels$activityCode)]
    
    # Append activities to test data. Note, to include the activity code, uncomment the cbind line and comment out the line below it.
    #test <- cbind(test, testActivities$activityLabel)
    test$activity <- testActivities$activityLabel
    
    # Read test subjects.
    subjects <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, nrows = nrows, col.names = c("subject"), na.strings = "", stringsAsFactors = FALSE)

    # Append subjects to test data.
    test <- cbind(test, subjects)
    
    # Merge the tables together.
    data <- rbind(train, test)
}

#
# Extracts all columns from the data set that relate to mean and standard deviation features.
#
createMeanStdData <- function(data) {
    # Remove all columns except for activity, mean, and standard deviations.
    data <- data[, grep("activity|[Mm]ean|[Ss]td", names(data))]

    # Return result.
    data
}

#
# Creates a tidy data set with the average of each variable for each activity and subject.
#
createTidyData <- function(data) {
    # Create a tidy data frame for the result.
    result <- data.frame(stringsAsFactors = FALSE)

    # Split the data into groups by subject.
    dataBySubject <- split(data, data$subject)
    
    # Split the group further into groups by activity.
    dataBySubjectAndActivity <- lapply(dataBySubject, function(e) split(e, e$activity))

    # For each subject in length(dataBySubjectAndActivity), enumerate each activity length(dataBySubjectAndActivity[[1]]).
    # Iterate over each subject/activity group.
    lapply(dataBySubjectAndActivity, function(subjectGroup) {
        # Iterate over each activity in the group.
        lapply(subjectGroup, function(subjectActivityGroup) {
            # Create a row. Note, we put the subject and activity at the front.
            row <- data.frame(subject = integer(), activity = character(), stringsAsFactors = FALSE)
            
            # Iterate over each column by index. This way we can access the column names as well as the data.
            lapply(seq_along(subjectActivityGroup), function(i) {
                if (i < 562) {
                    # This is a feature column, so calculate the mean for this subject/activity and set the value in the cell.
                    row[1, names(subjectActivityGroup[i])] <<- mean(subjectActivityGroup[[i]])
                }
                else {
                    # This is a subject or activity column. Set the value in the cell.
                    row[1, names(subjectActivityGroup[i])] <<- subjectActivityGroup[[i]][1]
                }
            })
            
            # Append the resulting row to our result data frame.
            result <<- rbind(result, row)
        })
    })
    
    # Return result.
    result
}

# Load train and test data into a single data set.
data <- loadData()

# Get all columns relating to mean and standard deviation.
meanStdData <- createMeanStdData(data)

# Create a tidy data set with the average of each variable for each activity and each subject.
tidyData <- createTidyData(data)

# Write output file 1.
write.csv(meanStdData, "meanStd.csv", quote=FALSE, row.names=FALSE)

# Write output file 2.
write.csv(tidyData, "tidy.csv", quote=FALSE, row.names=FALSE)
