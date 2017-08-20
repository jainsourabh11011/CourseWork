#quiz4

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(fileUrl,destfile="./data/quiz4.pdf",method="curl")

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="./data/quiz4.csv",method="curl")

t <- read.csv("./data/quiz4.csv")
dim(t)
strsplit(names(t),"wgtp")[123]

#2
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl,destfile="./data/quiz4_2.csv",method="curl")
# modify manually
t2 <- read.csv("./data/quiz4_2.csv",header=F)
#t2 <- t2[!is.na(t2$V2) && !is.na(t2$V2),] # ranked only
t2$GDP <- sapply(t2$V5,FUN=function(x) gsub(",","",x))
t2$GDP <- sapply(t2$GDP,FUN=function(x) gsub(" ","",x))
mean(as.numeric(t2$GDP))

#3
length(sapply(t2$V4,FUN=function(x) grep(".*United",x))>0)
for (i in t2$V4) {
  #cat(i,"\n")
  if (length(grep("^United",i)) > 0 ){
    cat("found",i,"\n")
  }
}

#4
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl,destfile="./data/quiz4_3.csv",method="curl")
t3 <- read.csv("./data/quiz4_3.csv")
dim(t3)
r <- merge(t2,t3,by.x="V1", by.y="CountryCode",all=F,na.rm=T)
str(r)
dim(r)
r[grep ("Fiscal year end: June", r$Special.Notes),"Special.Notes"]


#4
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
length(sampleTimes)
length(sampleTimes[format(sampleTimes,"%Y") == "2012"])
length(sampleTimes[format(sampleTimes,"%Y") == "2012" & format(sampleTimes,"%a") == "Mon"])
