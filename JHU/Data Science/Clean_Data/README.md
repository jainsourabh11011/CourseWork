# Johns Hopkins Data Scientists Certification
## Getting and Cleaning Data

**Project Introduction**

The purpose of this project is to demonstrate the ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here is the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

**Project Deliverables**

This project includes these activities: 

1. a tidy data set as described below
2. a link to a Github repository with your script for performing the analysis
3. a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
4. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  (You are reading this document now;)

An R script called run_analysis.R does this:
 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set.
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

Steps to reproduce this project

1. First, from your working directory, download this file:https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Verify the files have been downloaded, unzipped and in the following folder structure:  ./data/Class3Project/
3. Fork run_analysis.R script from my Github account: https://github.com/ZellW/Class3Project
3. Run "run_analysis.R" command in RStudio. 
4. Use data <- read.table("./data/Class3Project/tidyData.txt") command in RStudio to read the file. 

The project averaged each measurement for each activity and each subject.  There were 30 people in the study that performed 6 different activities (like walking, sitting) and 66 measurements recoded from the accelerometers from  Samsung Galaxy S smartphones.

**Outputs**

* Tidy dataset file tidyData.txt

To view the data in R, use the following code:
````
address<-"https://s3.amazonaws.com/coursera-uploads/user-387751591c2a5e160aea6373/973502/asst-3/5922f6900ef811e5a3269be689cef053.txt"
address <- sub("^https", "http", address)
data <- read.table(url(address), header = TRUE) 
View(data)
````
