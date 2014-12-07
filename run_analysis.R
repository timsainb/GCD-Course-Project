# Cleaning Data Script
# Tasks
# 1. Merge training and test sets to create one data set
# 2. Extract only the measurements on the mean and standard deviation for each measurement
# 3. Use descriptive activity names to name the activities in the dataset
# 4. Appropriately label the deta set with descriptive variable names
# 5. From the data set in step 4, create a second, independant tidy 
#     dataset with the average of each variable for each activity and each subject


create_data_set <- function(directory) {
# load test and train data
testdata = read.table("UCI HAR Dataset/test/X_test.txt")
traindata = read.table("UCI HAR Dataset/train/X_train.txt")
# gets y values
ytest = read.table("UCI HAR Dataset/test/Y_test.txt")
ytrain = read.table("UCI HAR Dataset/train/Y_train.txt")
#bind yvalues
allY <- rbind(ytest, ytrain)
# get descriptions for Y values
ydesc = read.table("UCI HAR Dataset/activity_labels.txt")
allY <- merge(allY, ydesc, by="V1")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
allSubject <- rbind(subject_test, subject_train)

#merge data together
alldata <- rbind(testdata, traindata)

#removes old data sets
rm(testdata)
rm(traindata)

#gives names to features
namesOfFeatures = read.table("UCI HAR Dataset/features.txt")
colnames(alldata) <- namesOfFeatures$V2

#finds columns which contain the words mean, or std
columnsToExtract <- grep("mean|std",colnames(alldata))

#creates new data with only columns that have mean of std
alldata <- alldata[,columnsToExtract]

#applies column names
colnames(alldata) <- namesOfFeatures$V2[columnsToExtract]

# Appends Activity Type and Number to data
alldata$ActivityNumber <- allY$V1
alldata$ActivityType <- allY$V2
alldata$Subject <- allSubject$V1

return (alldata)
}

# creates new tidy dataset with average of each variable for each activity and each subject
tidydataset <- function(dataset) {
    
  #splitDataSet <- split(dataset, list(dataset$ActivityNumber,dataset$Subject))
  
  require(reshape2)
  melted_data <- melt(dataset, id=c("Subject","ActivityNumber","ActivityType"))
  
  # cast data into tidy data format
  tidy_data <- dcast(melted_data, formula = Subject + ActivityNumber + ActivityVariable ~ variable, mean)
  
  write.table(tidy_data, file="./tidy_data.txt")
}


