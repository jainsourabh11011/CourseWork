## Exploratory Data Analysis - Plotting Assignment 2 - plot2

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
#Subset for Baltimore - fips = 24510
Balt_NEI <- subset(sourceNEI, sourceNEI$fips == "24510")
#This file not used in exercise
#SCC <- readRDS("Source_Classification_Code.rds")


png(filename = "plot2.png")  
     
y <- Balt_NEI$Emissions/100000
x <- Balt_NEI$year
graphdata <- tapply(y, x, FUN = sum)
     
barplot(graphdata, main = expression('Emissions of PM'[2.5] ~ ' in Baltimore, MD'), 
        ylab = expression('Total PM'[2.5] ~ 'emission (tons x 100,000)'), 
        xlab = "Year")
     
lines(graphdata * 0.8, col = "blue", lwd = 2)
points(graphdata * 0.8, col = "blue", lwd = 5)
     
dev.off()
     

