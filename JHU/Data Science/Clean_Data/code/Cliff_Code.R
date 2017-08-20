# Set working directory:
setwd("F:\\Skydrive\\Certification\\Data Science Specialization\\R_Working_Dir"
#
# Put files in ther data folder under the working directory 
if(!file.exists("./data")){dir.create("./data")} #If the data folder does not exist, create it
#
# Get the data.  zip file from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#
download.file(url1,destfile="./data/Class3Project.zip")
unzip(zipfile="./data/Class3Project.zip",exdir="./data/Class3Project")
#
# Files in the Inertial Signals folder are not used. 
# THese are the only files we need to work with.
#      
# test/subject_test.txt - a number represented the person involved with the test
# test/X_test.txt - the data collected and caluculated in XYZ dimensions
# test/y_test.txt - The activity the t4est subject was doing when being measures (like walking)

# train/subject_train.txt - a number represented the person involved with the test
# train/X_train.txt - the data collected and caluculated in XYZ dimensions
# train/y_train.txt - The activity the t4est subject was doing when being measures (like walking)

# features.txt - contains the names of the measurements recorded like tBodyAcc-entropy()-X
# activity_lables.txt - The names of the activities like standing, walking, lying

# Read in the data from files

# Shared files for Test and Train
features     = read.table('./data/Class3Project/features.txt',header=FALSE); #imports features.txt
activitylabels = read.table('./data/Class3Project/activity_labels.txt',header=FALSE); #imports activity_labels.txt

# Training Files
trainsubject = read.table('./data/Class3Project/train/subject_train.txt',header=FALSE); #imports subject_train.txt
trainxdata = read.table('./data/Class3Project/train/x_train.txt',header=FALSE); #imports x_train.txt
trainydata = read.table('./data/Class3Project/train/y_train.txt',header=FALSE); #imports y_train.txt

# Test Files
testsubject <- read.table("./data/Class3Project/test/subject_test.txt")
testxdata <- read.table("./data/Class3Project/test/X_test.txt")
testydata <- read.table("./data/Class3Project/test/Y_test.txt")

# Class Project Object 1:  Merge data by rows then columns
xdatacombined <- rbind(trainxdata, testxdata)
ydatacombined <- rbind(trainydata, testydata)
subjectdatacombined <- rbind(trainsubject, testsubject)

# Class Project Objective 3
# Before completing the dataset, let's change the ydatacombined to hold not numbers (factors) but descriptive text
activitylabels[, 2] = gsub("_", " ", tolower(as.character(activitylabels[, 2])))
ydatacombined[,1] = activitylabels[ydatacombined[,1],2]

names(ydatacombined) <- "activity"

completedataset <- cbind(subjectdatacombined, ydatacombined, xdatacombined) 
# (Order is important so subject and activity are first two columns)

# Name the measurement columns using the feature names in column 2
# First get the feature names (like tBodyAcc-mean()-Z)
getfeaturenames <- as.character(features[,2])
# add common sense column names for the test subject - people in the study - and the type of activity - like walking
newcolnames <- c("subject", "activity", getfeaturenames)
# assign the column names to the completed data set
colnames(completedataset) <- newcolnames

# Let's get rid of some of the data that we no longer need
rm(trainsubject, trainxdata, trainydata, testsubject, testxdata, testydata, xdatacombined, ydatacombined, subjectdatacombined)

# Class Project Objective 2:  Extract the mean and st.dev from the completed data set
# In the complete data set, there are many column in addition to mean and st dev like energy, iqr, correlation, entropy
# Want to remove all columns other than thos containing mean() or std()
onlymean <- grep("mean()", colnames(completedataset))
onlystdev <- grep("std()", colnames(completedataset))

meanstddevcolumnindex <- c(onlymean, onlystdev)
meanstddevcolumnindexsorted <- sort(meanstddevcolumnindex) 
FinalDataSet <- completedataset[, c(1,2,meanstddevcolumnindexsorted)]

# Let's get rid of some of the data that we no longer need (one of them is large)
rm(onlymean, onlystdev, meanstddevcolumnindex, meanstddevcolumnindexsorted, completedataset)

# Class Project Objective 3 & 4: Use descriptive activity names to name the activities in the data set
# NOTE - Objective 3 has already been completed earlier in the code (line 54)
# The data contains cryptic values such as:
# tBodyAcc-entropy()-X
# tGravityAcc-energy()-X
# fBodyAcc-std()-Z
# angle(tBodyGyroJerkMean,gravityMean)
# 
# Lets try to make these more user-friendly using the data documentation as a guide
# We have already made the activity column easily understood.  We changed the factors to the actual
# textual descriptions given in the activity_labels.txt file

names(FinalDataSet)<-gsub("^t", "Time", names(FinalDataSet)) 
names(FinalDataSet)<-gsub("^f", "Frequency", names(FinalDataSet))
names(FinalDataSet)<-gsub("Acc", "Accelerometer", names(FinalDataSet))
names(FinalDataSet)<-gsub("Gyro", "Gyroscope", names(FinalDataSet))
names(FinalDataSet)<-gsub("Mag", "Magnitude", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", names(FinalDataSet))
names(FinalDataSet)<-gsub("-std", "StdDev", names(FinalDataSet))
names(FinalDataSet)<-gsub("-mean", "Mean", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Gg]ravity)", "Gravity", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", names(FinalDataSet))

# Class Objective 5: From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
# See this for good explanation: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
# http://www.r-bloggers.com/aggregate-function-in-r-making-your-life-easier-one-mean-at-a-time/

tidydataset<-aggregate(. ~subject + activity, FinalDataSet, mean)
tidydataset<-tidydataset[order(tidydataset$subject,tidydataset$activity),]

write.table(tidydataset, file = "./data/Class3Project/tidyData.txt",row.names=FALSE);
