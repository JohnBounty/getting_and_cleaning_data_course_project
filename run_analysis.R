#Here are the data for the project: 
#
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#Setting wd
rm(list=ls())
setwd("/Users/mac/Desktop/Coursera/UCI\ HAR\ Dataset")

# reading data

features <- read.table("./features.txt", header = FALSE)
actType<- read.table("./activity_labels.txt", header = FALSE)
#Train
subjectTrain<-read.table("./train/subject_train.txt", header = FALSE)
xTrain<-read.table("./train/x_train.txt", header = FALSE)
yTrain<-read.table("./train/y_train.txt", header = FALSE)
#Test
subjectTest<-read.table("./test/subject_test.txt", header = FALSE)
xTest<-read.table("./test/x_test.txt", header = FALSE)
yTest<-read.table("./test/y_test.txt", header = FALSE)

# merging data

colnames(actType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

trainingData <- cbind(yTrain, subjectTrain, xTrain)
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)   = "activityId"

testData<-cbind(yTest, subjectTest, xTest)
finalData<-rbind(trainingData, testData)
# Cleaning data
colNames  = colnames(finalData) 
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData <- finalData[logicalVector==TRUE]

#Using descriptive activity names
finalData <- merge(finalData,actType,by='activityId',all.x=TRUE)
colNames  = colnames(finalData) 
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(finalData)<-colNames

#creating new table of averages

finalDataNoActivityType  <- finalData[,names(finalData) != 'actType']
tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

tidyData    <- merge(tidyData,actType,by='activityId',all.x=TRUE)
# Making a new file with the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')

