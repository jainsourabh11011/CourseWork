#Load dplyr
library(dplyr)#Required for some of the steps below

if (!file.exists("data/activity.zip")) {
     download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="data/activity.zip")
     unzip("data/activity.zip")
     }
activity<- read.table(unzip("data/activity.zip"), header=TRUE, sep=",", na.strings="NA")

glimpse(activity)
activity$date<-as.Date(activity$date, "%Y-%m-%d")
glimpse(activity)

# convert to local data frame. Printing only shows 10 rows and as many columns as can fit on your screen
activity <- tbl(activity) #Not currently used in the Rmd

#Original code
steps_day <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)#Is this even used anywhere?
activityDate <- group_by(activity, date)
activityDateStep <- summarize(activityDate, total=sum(steps, na.rm=TRUE))

#Modified Code
activityDateStep <- activity %>% group_by(date) %>% summarize(total=sum(steps, na.rm=TRUE))
identical(activityDateStep, activityDateStep)#TRUE

#Original Code
hist(activityDateStep$total, main="Frequency of the Number of Steps Taken Daily", xlab="Steps", breaks=15)
abline(v=mean(activityDateStep$total), lty=4, col="red")
text(9500, 14, "Mean",col="red", pos=2)
text(9500,13,format(mean(activityDateStep$total), digits=1),col="red", pos=2)
abline(v=median(activityDateStep$total), lty=2, col="blue")
text(10100, 14, "Median",col="blue", pos=4)
text(10100, 13, median(activityDateStep$total),col="blue", pos=4)

aveSteps <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
#############Modified Line above Code################
aveSteps <- activity %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm=TRUE))
identical(aveSteps, as.data.frame(aveSteps2))#TRUE
######################################################
plot(aveSteps, type="l", main="Time Series: Average Number of Steps", xlab="5-Minute Interval", 
     ylab="Ave steps taken")
maxInterval<-aveSteps[which.max(aveSteps$steps),1]
abline(v=maxInterval, lty=2, col="blue")
text(maxInterval, 180, "Interval - Max Steps",col="blue", pos=4)
text(maxInterval, 170, maxInterval, col="blue", pos=4)
###############Original Code##############
maxSteps <- round(aveSteps[which(aveSteps$interval==maxInterval),2],1)
#################New Code########################
maxSteps <- filter(aveSteps, interval==maxInterval$interval)
maxSteps <- round(maxSteps[,2],0)
#########################################

sumNA <- sum(is.na(activity$steps))

activityNEW <- activity #Make copy of the data
#Goup data by interval and then find the interval mean
####################THIS WORKS####################
# activityInterval <- group_by(activityNEW, interval)
# #Create new DF with step averages for each interval
# activityIntervalStepAve <- summarize(activityInterval, average=mean(steps, na.rm=TRUE))
# activityNEW$average <- activityIntervalStepAve$average[match(activityNEW$interval, activityIntervalStepAve$interval)]
# activityNEW <-mutate(activityNEW, steps=replace(activityNEW$steps, is.na(activityNEW$steps), activityNEW$average))
###################################################

###############NEW CODE##############################
activityIntervalStepAve <- activityNEW %>% group_by(interval) %>% summarize(average=mean(steps, na.rm=TRUE))
activityIntervalStepAve %>% sample_n(5) #Take a look at 5 random records
#The code below retrieves the position of the average steps for the interval in the second argument that is equal to
#the interval in the dataset activityNEW$interval.This position number is then used to subset the 2nd argument to get 
#the average step value and put that in the newcolumn called average in the dataset actvityNEW
activityNEW$average <- activityIntervalStepAve$average[activityIntervalStepAve$interval %in% activityNEW$interval] #Does this work?
###################################################

activityNEW %>% sample_n(5)
#Where NA exists in the steps column, replace with the interval average
#Info on using replace with NA values found here:  http://rprogramming.net/recode-data-in-r/
#Original code
activityNEW <-mutate(activityNEW, steps=replace(activityNEW$steps, is.na(activityNEW$steps), activityNEW$average))
#Modified code
activityNEW2 <- activityNEW %>% mutate(steps=replace(activityNEW$steps, is.na(activityNEW$steps), activityNEW$average)) #compare to above
identical(activityNEW, activityNEW2) #True
#Final Code
activityNEW <- activityNEW %>% mutate(steps=ifelse(is.na(activityNEW$steps),activityNEW$average, activityNEW$steps))
identical(activityNEW, activityNEW3)

activityNEW %>% sample_n(5)

############Original Code#################
#Develop the data like the histogram before
steps_day2 <- tapply(activityNEW$steps, activityNEW$date, FUN=sum)#Where is this used?
activityDate2 <- group_by(activityNEW, date)
activityDateStep2 <- summarize(activityDate2, total=sum(steps, na.rm=TRUE))
#activityDateStep2$total <- as.numeric(activityDateStep2$total)

############Modified Code#################
activityDateStep2 <- activityNEW %>% group_by(date) %>% summarize(total=sum(steps, na.rm=TRUE))
identical(activityDateStep2, activityDateStep3)#TRUE
##########################################

#Write the new histogram
hist(activityDateStep2$total, main="Frequency of the Number of Steps Taken Daily", xlab="Steps", breaks=15)
abline(v=mean(activityDateStep2$total), lty=4, col="red")
text(9500, 14, "Mean",col="red", pos=2)
text(9500,13,format(mean(activityDateStep2$total), digits=1),col="red", pos=2)
abline(v=median(activityDateStep2$total), lty=2, col="blue")
text(10100, 14, "Median",col="blue", pos=4)
text(10100, 13, format(median(activityDateStep2$total), digits=1),col="blue", pos=4)


# Calculate the weekdays
#day_type <- weekdays(as.Date(activityNEW$date)) DO NOT need.  Alreday took care of this in the beginning
dayType <- weekdays(activityNEW$date)
# Assign the weekdays and the weekends to the data set
dayType <- ifelse(test = dayType %in% c("Saturday", "Sunday"), yes="weekend", "weekday")
activityNEW$dayType <- as.factor(dayType)

####################Original Code###########################
aveStepsInt <- aggregate(steps~interval+day_type, data=activityNEW, FUN=mean)
###################Updated Code############################
aveStepsInt <- activityNEW %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm=TRUE))
identical(aveSteps, as.data.frame(aveSteps2))#TRUE
#############################################################

library(lattice)
xyplot(steps~interval|dayType, aveStepsInt, type="l", layout=c(1,2)
       , main="Time series of the 5-minutes interval and the average \
       number of steps taken during weekdays and weekends")


    
########################################
days <- weekdays(filledInData$date)
weekend <- (days == "Saturday" | days == "Sunday")
dayfactor <- factor(weekend, labels = list("weekday", "weekend"))
filledInData$daytype <- dayfactor

groupedData <- aggregate(filledInData$steps, list(DayType = filledInData$daytype, 
                                                  Interval = filledInData$interval), mean)
library(lattice)
xyplot(groupedData$x ~ groupedData$Interval | groupedData$DayType, layout = c(1, 
                                                                              2), xlab = "Interval", ylab = "Number of Steps", type = "l")
       
###########################
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
     steps.type <- aggregate(steps ~ interval, data = activityNEW, subset = activityNEW$dayType == 
                                  type, FUN = mean)
     plot(steps.type, type = "l", main = type)
}

       