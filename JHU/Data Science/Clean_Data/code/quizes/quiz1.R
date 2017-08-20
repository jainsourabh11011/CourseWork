#quiz1
t <- read.csv('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv') 
names(t)
nrow(t[,"VAL"])
str(is.t$VAL)
str(na.omit(t$VAL))
d <- t$VAL[!is.na(t$VAL)]
str(d)
length(d[which(d==24)])

if(!file.exists("data")){dir.create("data")}
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl,destfile="./data/NGAP.xlsx",method="curl")
dateDownloaded <- date()

library(xlsx)
t2 <- read.xlsx("./data/NGAP.xlsx",sheetIndex=1,header=TRUE)
head(t2)
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("./data/NGAP.xlsx",sheetIndex=1,
                              colIndex=colIndex,rowIndex=rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T) 

library(XML)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileUrl,destfile="./data/resto.xml",method="curl")
doc <- xmlTreeParse("./data/resto.xml",useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
nodes <- rootNode[names(xmlChildren(rootNode)) == "row"]
t3 <- xpathSApply(rootNode,"//zipcode",xmlValue)
str(t3)
length(which(t3==21231))
length(t3)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl,destfile="./data/pid.csv",method="curl")
t4 <- read.csv("./data/pid.csv")
str(t4)

library(data.table)
DF = fread("./data/pid.csv")
str(DF)
DF[,ave:= mean(pwgtp15),by=SEX]
head(DF,3)
