## Exploratory Data Analysis - Plotting Assignment 2 - plot4

## Download the  data and put it the working directory
# Set working directory:
setwd(".\\Class4Project2")

sourcefile = "Source_Classification_Code.rds"

# Put files in the working directory 
if (!file.exists(sourcefile)) {
     url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
     f <- file.path(getwd(), "Class4Project1.zip")
     download.file(url1,f)
     unzip(zipfile="Class4Project2.zip")
     file.remove("Class4Project2.zip")
}

# read the original data
sourceNEIdata <- readRDS("summarySCC_PM25.rds")
#Subset for Baltimore - fips = 24510

#This file not used in exercise
sourceSCCdata <- readRDS("Source_Classification_Code.rds")

library(plyr)
library(ggplot2)

# Subset data
coal <- grep("coal",sourceSCCdata$EI.Sector,value=T,ignore.case=T) 
SRC.coal <- subset(sourceSCCdata, sourceSCCdata$EI.Sector %in% coal, select=SCC) 
NEI.coal <- subset(sourceNEIdata, sourceNEIdata$SCC %in%  SRC.coal$SCC) 
graphdata <- aggregate(NEI.coal[c("Emissions")], list(year = NEI.coal$year), sum) 
     
png(filename = "plot4.png")  
     
graph <- ggplot(graphdata) + aes(x = factor(year), 
     y = Emissions/100000, group = 1) + geom_point(size = 4) + geom_line() +
     labs(title = expression('Total PM'[2.5] ~ ' Coal Combustion Emissions in the US'), 
     x= "Year", y = expression("Total PM"[2.5] ~ "emission (tons x 10^6)")) 
     
print(graph)
dev.off()
     

