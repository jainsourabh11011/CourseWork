#Wierd error:  ddply fails with: Error: argument "by" is missing, with no default
## In the help file the ddply function call should say "summarise" instead of "summarize".
##May have occurred because Hmisc was loaded last
library(Hmisc)
library(dplyr)#Required for some of the steps below
library(ggplot2)#Required for some of the steps below
library(tidyr)#Required for some of the steps below
library(lubridate)#Required for some of the steps below

if (!file.exists("./Class5Assign2data/workingData.csv")) {
     download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                   destfile="./Class5Assign2/data/rawData.csv.bz2")
     file = bzfile('./Class5Assign2/data/rawData.csv.bz2')
     rawData <- read.csv(file)
     #Let's preserve the raw data and creat a working copy of the dataset
     workingData <- rawData %>% select(BGN_DATE, EVTYPE, FATALITIES, INJURIES,PROPDMG,PROPDMGEXP, CROPDMG, CROPDMGEXP)
     #Let's save a copy of the working data
     write.csv(workingData, "./Class5Assign2/data/workingData.csv")
}
#If the workingData.csv exists, let's open it for analysis:
workingData <- read.csv("./Class5Assign2/data/workingData.csv", na.strings=c('?', "", "-"), stringsAsFactors = FALSE)
#Because weather reporting early in the dataset was sporatic, we are going to evalaute wehtehr events later than 1995
workingData$BGN_DATE <- mdy_hms(workingData$BGN_DATE)
workingData <- workingData %>% filter(year(BGN_DATE) > 2000)
# convert to local data frame. Printing only shows 10 rows and as many columns as can fit on your screen
workingData <- tbl_df(workingData)
glimpse(workingData)

#In the fields, the format of the data is not consistent.  Here are a few examples:
unique(workingData$PROPDMGEXP)
unique(workingData$CROPDMGEXP)
#This needs to be corrected.

#workingData$EVTYPE <- as.factor(workingData$EVTYPE)
workingData$PROPDMGEXP <- toupper(workingData$PROPDMGEXP)
workingData$PROPDMGEXP[workingData$PROPDMGEXP == ""] <- "0"
workingData$CROPDMGEXP <- toupper(workingData$CROPDMGEXP)
workingData$CROPDMGEXP[workingData$CROPDMGEXP == ""] <- "0"

unique(workingData$PROPDMGEXP)

#Property damage
workingData$PROPDMGEXP <- gsub('K', '1000', workingData$PROPDMGEXP)
workingData$PROPDMGEXP <- gsub('M', '1000000', workingData$PROPDMGEXP)
workingData$PROPDMGEXP <- gsub('M', '1000000000', workingData$PROPDMGEXP)
glimpse(workingData)

workingData$PROPDMGEXP <- as.numeric(workingData$PROPDMGEXP, na.rm=TRUE)
#Multiply the PROPDMG values with their respective PROPDMGEXP values
workingData$PROPDMG <- workingData$PROPDMG * workingData$PROPDMGEXP

#Crop Damage
workingData$CROPDMGEXP <- gsub('K', '1000', workingData$CROPDMGEXP)
workingData$CROPDMGEXP <- gsub('M', '1000000', workingData$CROPDMGEXP)
workingData$CROPDMGEXP <- gsub('B', '1000000000', workingData$CROPDMGEXP)
workingData$CROPDMGEXP <- as.numeric(workingData$CROPDMGEXP)

#Multiply the CPROPDMG values with their respective CPROPDMGEXP values
workingData$CROPDMG <- workingData$CROPDMG * workingData$CROPDMGEXP


totals <- workingData %>%
     group_by(EVTYPE) %>%
     summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES),PROPDMG = sum(PROPDMG, na.rm = TRUE),
               CROPDMG = sum(CROPDMG, na.rm = TRUE)) %>%
     mutate(totalDeathInjury=FATALITIES + INJURIES, totalDollarLost= PROPDMG + CROPDMG)

#totalsPlot1 <- totalsPlot1 %>% select(EVTYPE,FATALITIES, INJURIES, totalDeathInjury)
totalsPlot1 <- totals[order(-totals$totalDeathInjury)[1:10],]

ggplot(totalsPlot1, aes(x = reorder(EVTYPE, -totalDeathInjury), y = totalDeathInjury)) +
     geom_bar(stat = "identity", fill = "lightblue") +
     theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
     xlab(NULL) + ylab("Damage in Death and Injury") +
     ggtitle(paste("Death and Injury: Weather Events"))


totalsPlot2 <- totals[order(-totals$totalDollarLost)[1:10], ]

ggplot(totalsPlot2, aes(x = reorder(EVTYPE, -totalDollarLost), y = totalDollarLost/1000000000)) +
     geom_bar(stat = "identity", fill = "lightblue") +
     theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
     xlab(NULL) + ylab("Damage in $ - Billions") +
     ggtitle(paste("Largest Economic Losses: Weather"))

