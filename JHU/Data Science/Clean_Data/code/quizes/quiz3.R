#quiz3

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(fileUrl,destfile="./data/quiz3.pdf",method="curl")


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="./data/quiz3.csv",method="curl")
t <- read.csv("./data/quiz3.csv")
dim(t)
agricultureLogical <- (t$ACR == 3 & t$AGS == 6)
str(agricultureLogical)
head(which(agricultureLogical),3)

#2
library(jpeg)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl,destfile="./data/quiz3.jpg",method="curl")
i <- readJPEG("./data/quiz3.jpg",native=TRUE)
str(i)
dim(i)
quantile(i,c(.30,.80))

#3
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl,destfile="./data/quiz3_2.csv",method="curl")
# modify manually
t2 <- read.csv("./data/quiz3_2.csv",header=F)
#t2 <- t2[!is.na(t2$V2) && !is.na(t2$V2),] # ranked only

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl,destfile="./data/quiz3_3.csv",method="curl")
t3 <- read.csv("./data/quiz3_3.csv")
  
dim(t2)
dim(t3)
str(t2)
str(t3)
unique(t2$V1)
unique(t3$CountryCode)

calc <- function (){
n <- 0
for (i in t2$V1) {
  for (j in t3$CountryCode) {
    if (i == j) {
      n= n + 1
      break
    }
  }
}
return(n)
}
print(calc())  
r <- merge(t2,t3,by.x="V1", by.y="CountryCode",all=F,na.rm=T)
str(r)
dim(r)
r[order(r$V2,decreasing=T),][13,c("V1","V4")]

#4
mean(r[r$Income.Group == "High income: OECD","V2"])
mean(r[r$Income.Group == "High income: nonOECD","V2"])

library(Hmisc)
table(cut2(r$V2,g=5),r$Income.Group)

table(cut2(r$V2,g=5),r$Income.Group)
table(quantile(r$V2,c(.2,.4,.6,.8)),r$Income.Group)
ftable(cut(r$V2,5),r$Income.Group)
addmargins(ftable(cut(r$V2,5),r$Income.Group))