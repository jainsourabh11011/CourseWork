## Coursera - Exploratory Data Analysis - Plotting Assignment 1
##
## plot2.R

## Download the  data and put it the working directory
# Set working directory:
setwd(".\\Class4Project1")

filename = "household_power_consumption.txt"

# Put files in the working directory 
if (!file.exists(filename)) {
     url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
     f <- file.path(getwd(), "Class4Project1.zip")
     download.file(url1,f)
     unzip(zipfile="Class4Project1.zip")
     file.remove("Class4Project1.zip")
}

# read the original data
RawData<-read.table("./household_power_consumption.txt", header=T,sep=";",na.strings = "?")
# ignore NA value
NoNA.RawData<-na.omit(RawData)
# subset the data
ElectricPwr<-subset(NoNA.RawData,Date=="1/2/2007"|Date=="2/2/2007")
# converting the date
ElectricPwr$Date<-as.Date(ElectricPwr$Date,format="%d/%m/%Y")
# add new date column
ElectricPwr$DwT<-paste(ElectricPwr$Date,ElectricPwr$Time)
# convert the date using as.POXIXct()
ElectricPwr$DwT <- as.POSIXct(ElectricPwr$DwT)
# create the gragh
# Note: an expression with a tilde ("~") which indicates the relationship between the input variables. 
# This allows you to enter something like mpg ~ cyl to plot the relationship between number of cylinders on the 
# x-axis and mpg on the y-axis.
plot(ElectricPwr$Global_active_power~ElectricPwr$DwT, type="l",ylab="Global Active Power (kilowatts)", xlab="")
# save PNG file
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()
