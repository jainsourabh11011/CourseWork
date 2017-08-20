## Exploratory Data Analysis - Plotting Assignment 2 - plot5

library(ggplot2)

# Work with data frames 
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html 
library(plyr) 

## Download the  data and put it the working directory
# Set working directory:
setwd(".\\Class4Project2")

# Put files in the working directory 
if (!file.exists("Source_Classification_Code.rds")) {
     url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
     f <- file.path(getwd(), "Class4Project1.zip")
     download.file(url1,f)
     unzip(zipfile="Class4Project2.zip")
     file.remove("Class4Project2.zip")
}

# read the original data
SourceNEI <- readRDS("summarySCC_PM25.rds")
SourceSCC <- readRDS("Source_Classification_Code.rds")

#  Create Data
VEH <- grep("vehicle",SourceSCC$EI.Sector,value=T,ignore.case=T)
SourceSCC.VEH <- subset(SourceSCC, SourceSCC$EI.Sector %in% VEH, select=SCC)
Baltimore <- subset(SourceNEI, fips == "24510")
SourceNEI.VEH <- subset(Baltimore, Baltimore$SCC %in%  SourceSCC.VEH$SCC)
graphdata <- aggregate(SourceNEI.VEH[c("Emissions")], list(year = SourceNEI.VEH$year), sum)

#  Create Plot
png(filename="plot51.png")
# ggplot(graphdata$year, graphdata$Emissions, type = "l", 
#      main = "Total Vehicle Emissions in Baltimore City",
#      xlab = "Year", ylab = "Emissions")

graph <- ggplot(graphdata) + aes(x = graphdata$year, 
     y = graphdata$Emissions, group = 1) + geom_point(size = 4) + geom_line() +
     labs(title = expression('Motor Vehicle Emissions in Baltimore'), 
     x= "Year", y = expression("Emission (tons x 10^6)"))

print(graph)

dev.off()
