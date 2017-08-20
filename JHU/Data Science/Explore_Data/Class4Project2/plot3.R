## Exploratory Data Analysis - Plotting Assignment 2 - plot3

#Instructed to use ggplot2
library(ggplot2)

# Work with data frames 
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html 
library(plyr) 

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
sourceNEI <- readRDS("summarySCC_PM25.rds")
#Subset for Baltimore - fips = 24510
Balt_NEI <- subset(sourceNEI, sourceNEI$fips == "24510")
#This file not used in exercise
#SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot3.png")
myplot_data <- ddply(Balt_NEI, .(year, type), numcolwise(sum))
myplot <- ggplot(plot_data) + aes(x = factor(year), y = Emissions, 
     group = type, col = type) + geom_line() + geom_point() + xlab("Year")
print(myplot) 
dev.off()
