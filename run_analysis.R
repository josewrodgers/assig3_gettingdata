################ File Name:run_analysis.R#############
## Tidy Data 
## 1. each variable should be in one column
## 2. each observation of that variable should be in a diferent row
## 3. include ids to link tables together

# The packages we'll use are plyr and reshape2


#installing packages
packageName="plyr"
if(!is.element(packageName, installed.packages()[,1])){
        print("Installing package")
        install.packages(packageName)
}
# The library of the package
library(plyr)

packageName="reshape2"
if(!is.element(packageName, installed.packages()[,1])){
        print("Installing package")
        install.packages(packageName)
}
# The library of the package
library(reshape2)




#Unzipping and storing data 

file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- "UCI HAR Dataset"


## Create data and folders   
# verifies the data zip file has been downloaded
if(!file.exists(file)){
        
        ##Downloads the data file
        print("downloading Data")
        download.file(url,file, mode = "wb")
        
}

##unzip the file in wd

unzip (file)


#for renaming column names we need this function:
rename.features <- function(col) {
        col <- gsub("tBody", "Time.Body", col)
        col <- gsub("tGravity", "Time.Gravity", col)
        
        col <- gsub("fBody", "FFT.Body", col)
        col <- gsub("fGravity", "FFT.Gravity", col)
        
        col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
        col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
        
        col <- gsub("\\-mean\\(\\)", ".Mean", col)
        col <- gsub("\\-std\\(\\)", ".Std", col)
        
        return(col)
}




#################STARTING WITH TASKS#######################


## 1. Merges the training and the test sets to create one data set

#loading Features
message("loading features.txt")
data.set <- list()
data.set$features <- read.table(paste(path, "features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)

#Loading activities
message("loading activity_features.txt")
data.set$activity_labels <- read.table(paste(path, "activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))


#loading Test
message("loading test set")
data.set$test <- cbind(subject=read.table(paste(path, "test", "subject_test.txt", sep="/"), col.names="Subject"),
                       y=read.table(paste(path, "test", "y_test.txt", sep="/"), col.names="Activity.ID"),
                       x=read.table(paste(path, "test", "x_test.txt", sep="/")))


#loading train
message("loading train set")
data.set$train <- cbind(subject=read.table(paste(path, "train", "subject_train.txt", sep="/"), col.names="Subject"),
                        y=read.table(paste(path, "train", "y_train.txt", sep="/"), col.names="Activity.ID"),
                        x=read.table(paste(path, "train", "X_train.txt", sep="/")))

summary(data.set)

## Extracts only the measurements on the mean and standard deviation for each measurement.

tidydata <- rbind(data.set$test, data.set$train)[,c(1, 2, grep("mean\\(|std\\(", data.set$features$name) + 2)]

## Uses descriptive activity names to name the activities in the data set

names(tidydata) <- c("Subject", "Activity.ID", rename.features(data.set$features$name[grep("mean\\(|std\\(", data.set$features$name)]))

## Appropriately labels the data set with descriptive activity names.

tidydata <- merge(tidydata, data.set$activity_labels, by.x="Activity.ID", by.y="id")
tidydata <- tidydata[,!(names(tidydata) %in% c("Activity.ID"))]


## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


tidy.mean <-aggregate(tidydata, by=list(activity = tidydata$Activity, subject=tidydata$Subject), mean)
tidy.mean$subject=NULL

write.csv(tidy.mean, file = "tidydata.mean.txt",row.names = FALSE)
write.csv(tidydata, file = "tidydata.txt",row.names = FALSE)
