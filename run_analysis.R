## read in test and train data, features, and subjects
testX <- read.table("./test/X_test.txt")
testY <- read.table("./test/y_test.txt")
features <- read.table("./features.txt")
activityLabels <- read.table("./activity_labels.txt")
testSubjects <- read.table("./test/subject_test.txt")
trainX <- read.table("./train/X_train.txt")
trainY <- read.table("./train/y_train.txt")
trainSubjects <- read.table("./train/subject_train.txt")

## make variables names on testX and trainX correspond to features
names(testX) <- features$V2
names(trainX) <- features$V2

## add testY to testX df as new column with numeric indicator of specific activity conducted
testX$activity <- testY$V1
## add trainY to trainX df as new column with numeric indicator of specific activity conducted
trainX$activity <- trainY$V1

## add testSubjects to testX df
testX$subject <- testSubjects$V1
length(unique(testX$subject))
## add testSubjects to trainX df
trainX$subject <- trainSubjects$V1

## create variable labeling obs as from the test or train set
testX$origin <- "test"
trainX$origin <- "train"

## merge testX and trainX
mergedData <- rbind(testX, trainX)

## add text version of activities conducted
mergedData$activity_label <- "empty"
for(i in 1:nrow(mergedData)){
        index <- mergedData$activity[i]
        activityLabeli <- activityLabels[index, 2]
        activityLabeliChar <- as.character(activityLabeli)
        mergedData$activity_label[i] <- activityLabeliChar 
}

## reduce mergedData to keep only those measurement columns which take a mean or sd

colNames <- names(mergedData)
colMean <- grep("mean\\(\\)", colNames)
colSd <- grep("std\\(\\)", colNames)
colSdNames <- colNames[colSd]
colMeanNames <- colNames[colMean]

numObs <- nrow(mergedData)
df <- data.frame(1:numObs)
for(i in 1:length(colMeanNames)){
        dfColName <- colMeanNames[i]
        colIndex <- colMean[i]
        dfData <- mergedData[ , colIndex]
        df[dfColName] <- dfData
}

for(i in 1:length(colSdNames)){
        dfColName <- colSdNames[i]
        colIndex <- colSd[i]
        dfData <- mergedData[ , colIndex]
        df[dfColName] <- dfData
}

df <- df[ , -1]
df["activity_labels"] <- mergedData$activity_label
df["subject"] <- mergedData$subject

## create header vector of column names for final tidy data frame
headerVec <- c()
dfcolNames <- names(df)
for(i in 1:length(dfcolNames)) {
        colName <- dfcolNames[i]
        if(colName != "subject" & colName != "activity_labels") {
                for(l in 1:nrow(activityLabels)) {
                        newColName <- paste(colName,"_", activityLabels[l, 2], sep = "")
                        headerVec <- c(headerVec, newColName)
                }
        }
}

## create newdf dataframe which will contain final tidy data
newdf <- data.frame(matrix(ncol = length(headerVec), nrow = 5))
colnames(newdf) <- headerVec

## loop through df and extract the mean of each measurement, by subject and by activity
activityVec <- activityLabels$V2
subjectVec <- unique(df$subject)
for(i in 1:length(subjectVec)) {
        for(j in 1:length(activityVec)) {
                xdf <- df[df$subject == subjectVec[i] & df$activity_labels == activityVec[j], ]
                for(k in 1:ncol(xdf)) {
                        
                        xdfColName <- names(xdf[k])
                        if(xdfColName != "subject" & xdfColName != "activity_labels") {
                                xdfColMean <- mean(xdf[ , k])
                                newdfColName <- paste(xdfColName, "_", activityVec[j], sep = "")
                                newdf[subjectVec[i], newdfColName] <- xdfColMean
                        }
                }
        }
}

print(newdf)
write.table(newdf, file = "tidydata.txt", sep = ",", row.names = FALSE, col.names = colnames(newdf))
