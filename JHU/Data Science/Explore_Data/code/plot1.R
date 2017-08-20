## Coursera - Exploratory Data Analysis - Plotting Assignment 1
##
## plot1.R

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
# creating the gragh 
hist(ElectricPwr$Global_active_power,main="Global Active Power",xlab="Global Active Power (kilowatts)",col="red")
# save PNG file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()


