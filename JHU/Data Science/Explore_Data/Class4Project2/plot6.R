## Exploratory Data Analysis - Plotting Assignment 2 - plot6

library(ggplot2)
# Work with data frames 
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html 
library(dplyr) 

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

#  Create Plot Data 
VEH <- grep("vehicle",SourceSCC$EI.Sector,value=T,ignore.case=T)
SourceSCC.VEH <- subset(SourceSCC, SourceSCC$EI.Sector %in% VEH, select=SCC)
Baltimore_LA <- subset(SourceNEI, fips == "24510"|fips == "06037")
SourceNEI.VEH <- subset(Baltimore_LA, Baltimore_LA$SCC %in%  SourceSCC.VEH$SCC)
graphdata <- aggregate(SourceNEI.VEH[c("Emissions")], list(fips = SourceNEI.VEH$fips, year = SourceNEI.VEH$year), sum)
graphdata$city <- rep(NA, nrow(graphdata))
graphdata[graphdata$fips == "06037", ][, "city"] <- "Los Angles County"
graphdata[graphdata$fips == "24510", ][, "city"] <- "Baltimore City"

#  Create Plot
png(filename="plot6.png")
mygraph <- ggplot(graphdata, aes(x=year, y=Emissions/1000, colour=city)) +
     geom_point(alpha=.3) +
     geom_smooth(alpha=.2, size=1, method="loess") +
     ggtitle("Vehicle Emissions in Baltimore vs. LA") + 
     ylab("Emmisions in tons (x10^3")

print(mygraph)

dev.off()
