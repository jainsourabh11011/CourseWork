## Exploratory Data Analysis - Plotting Assignment 2 - plot1

## Download the  data and put it the working directory
# Set working directory:
setwd(".\\Class4Project2")

filename = "Source_Classification_Code.rds"

# Put files in the working directory 
if (!file.exists(filename)) {
     url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
     f <- file.path(getwd(), "Class4Project1.zip")
     download.file(url1,f)
     unzip(zipfile="Class4Project2.zip")
     file.remove("Class4Project2.zip")
}

# read the original data
sourceNEI <- readRDS("summarySCC_PM25.rds")
#This file not used in exercise
#SCC <- readRDS("Source_Classification_Code.rds")

#Start the graph     
png(filename = "plot1.png")  
#Get the x and y values     
y <- sourceNEI$Emissions/100000
x <- sourceNEI$year
graphdata <- tapply(y, x, FUN = sum)

barplot(graphdata, main = expression('Emissions of PM'[2.5] ~ ' in the United States'), 
        ylab = expression('Total PM'[2.5] ~ 'emission (tons x 100,000)'), xlab = "Year")

lines(graphdata * 0.8, col = "blue", lwd = 2)
points(graphdata * 0.8, col = "blue", lwd = 5)
     
dev.off()


