library("plyr")

#initialize activity labels
activityLabels <- read.csv("activity_labels.txt", colClasses = "character", header=FALSE, sep=" ")

#find column for which variables are mean or std
columnNames <- read.csv("features.txt", colClasses = "character", sep=" ", header=FALSE)
meanAndStdColumnNames <- grep("*mean\\(\\)|std\\(\\)*", columnNames[,2])

#make a widhtList vector with 16 characters for needed columns and -16 to skip other columns
widthList <- vector("numeric", length = 561)
widthList[1:561] <- -16
widthList[meanAndStdColumnNames] <- 16
widthList[1] <- 17


#read test data with fixed width list
testData <- read.fwf("test/X_test.txt", skip=0, widths=widthList)
unlink(testData)
#name columns for test data
colnames(testData) <- columnNames[meanAndStdColumnNames, 2]
#Use descriptive activity names to name the activities in the data set
#convert activities id to activity labels and add them to test data
testActivities <- read.csv("test/y_test.txt", colClasses = "character", header=FALSE)
colnames(testActivities)[1] <- "activity"
testData <- cbind(sapply(testActivities, function(x) activityLabels[x, 2]), testData)
#add subject id to test data
testSubjects <- read.csv("test/subject_test.txt", colClasses = "numeric", header=FALSE)
colnames(testSubjects)[1] <- "subject"
testData <- cbind(testSubjects, testData)



#read training data with fixed width list
trainData <- read.fwf("train/X_train.txt", skip=0, widths=widthList)
unlink(trainData)
#name columns for training data
colnames(trainData) <- columnNames[meanAndStdColumnNames, 2]
#Use descriptive activity names to name the activities in the data set
#convert activities id to activity labels and add them to training data
trainActivities <- read.csv("train/y_train.txt", colClasses = "character", header=FALSE)
colnames(trainActivities)[1] <- "activity"
trainData <- cbind(sapply(trainActivities, function(x) activityLabels[x, 2]), trainData)
#add subject id to training data
trainSubjects <- read.csv("train/subject_train.txt", colClasses = "numeric", header=FALSE)
colnames(trainSubjects)[1] <- "subject"
trainData <- cbind(trainSubjects, trainData)

# Merge the training and the test sets to create one data set.
mergedData <- rbind(testData, trainData)

#rename column names to delete "-", "(" and ")" and then put them in lower case 
modifiedColumnNames <- gsub("\\-", "", names(mergedData))
modifiedColumnNames <- gsub("\\)", "", modifiedColumnNames)
modifiedColumnNames <- gsub("\\(", "", modifiedColumnNames)
modifiedColumnNames <- tolower(modifiedColumnNames)
colnames(mergedData) <- modifiedColumnNames

#Appropriately labels the data set with descriptive variable names
#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- ddply(mergedData, c("subject", "activity"), function(x) colMeans(x[,3:68]))
#export csv file
write.csv(tidyData,"tidy_data.csv", row.names=FALSE)

tidyData
