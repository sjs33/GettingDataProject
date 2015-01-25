library(dplyr)

# set filepath for data files
filePathPre <- "C:/Users/ssimk_000/Documents/Training/DataAnalysis/GetData/CourseProject/Data/UCI HAR Dataset/"

# get column information
activityLabels <- read.table(paste(filePathPre,"activity_labels.txt",sep=""),col.names=c("activityID","activityName"))
colNames <- read.table(paste(filePathPre,"features.txt",sep=""))

# read in test data
subject <- read.table(paste(filePathPre,"test/subject_test.txt",sep=""),col.names="subject")
activity <- read.table(paste(filePathPre,"test/y_test.txt",sep=""),col.names="activityID")
data <- read.table(paste(filePathPre,"test/X_test.txt",sep=""))

fileVector <- rep("test",nrow(subject))

# add variable names to activities data frame and merge the labels and values data frames
names(data) <- colNames[,2]

# merge the activity and activity key data files
z <-merge(activity,activityLabels,sort=FALSE)

# create the data file with test information
dataTest <- cbind(fileVector,subject,activity=z$activityName,data)


# read in train data
subject <- read.table(paste(filePathPre,"train/subject_train.txt",sep=""),col.names="subject")
activity <- read.table(paste(filePathPre,"train/y_train.txt",sep=""),col.names="activityID")
data <- read.table(paste(filePathPre,"train/X_train.txt",sep=""))

fileVector <- rep("train",nrow(subject))

# add variable names to activities data frame and merge the labels and values data frames
names(data) <- colNames[,2]

# merge the activity and activity key data files
z <-merge(activity,activityLabels,sort=FALSE)

# create the data file with test information
dataTrain <- cbind(fileVector,subject,activity=z$activityName,data)
dataFull <- rbind(dataTest,dataTrain)

#extract required columns
data1<-dataFull[,1:3]
data2<-dataFull[,(grepl("mean()", colnames(dataFull),fixed=TRUE)|grepl("std()", colnames(dataFull),fixed=TRUE))]
dataSelected <- cbind(data1,data2)

# calculate means grouped by activity and subject
result <- aggregate(dataSelected,by=list(dataSelected$subject,dataSelected$activity),FUN="mean")
result <- select(result,-c(3:5))   #remove extra columns
names(result)[1:2]<- c("subject","activity")  #rename columns
prefix = c(rep("",2),rep("avg_",nrow(result)-2))   #create prefix vector for column labels
names(result) <- paste(prefix,names(result),sep="")   #alter column labels