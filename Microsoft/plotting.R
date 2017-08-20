
library(dplyr)
####################################################################################
#1 Load Data
myLoanData <- read.csv("LoansTrainingSet.csv", header=TRUE, stringsAsFactors = FALSE)

#2 Remove obvious duplicates
myLoanData<- myLoanData[!duplicated(myLoanData),]

#3A R Script - Column Names
columnnames<-c("LoanID","CustomerID","LoanStatus","CurrentLoanAmount","Term","CreditScore","YearsInCurrentJob",
               "HomeOwnership","AnnualIncome","Purpose","MonthlyDebt","YearsOfCreditHistory","MonthsSinceLastDelinquent",
               "NumberOfOpenAccounts","NumberOfCreditProblems","CurrentCreditBalance","MaximumOpenCredit","Bankruptcies",
               "TaxLiens")
colnames(myLoanData)<-columnnames
rm(columnnames)
#Add special column to hold values to overwrite ML results.  0 - null, 1 - Charged Off, 2- Paid Full
myLoanData$SpecialCase <- as.integer(0)

myLoanData$Term <- as.factor(myLoanData$Term)

#Fix duplicates related to Credit Score and Annual Income
  tmpDupeList <- unique(myLoanData[duplicated(myLoanData$LoanID),])#Returns the individual(only 1 record) duplicate Cust_IDs

  tmpDupeListLoanID <- tmpDupeList[,1]#Provides the list of LoanIDs that have duplicates
  
  rm(tmpDupeList)
  #Subset master with the values in the duplicated Cust_ID DF
  tmpDupeDF <- filter(myLoanData, LoanID %in% tmpDupeListLoanID)#Got all the dupes in one DF, now remove them from master file
  myLoanDataLoanID <- filter(myLoanData, !LoanID %in% tmpDupeListLoanID)#All unique LoanIDs YEAH!!  Will RBIND later

  #rm(tmpDupeListLoanID)
  #Now work on the duplcate file
  tmpDupeDF <- arrange(tmpDupeDF, desc(LoanID))

  tmpDupeDF <- filter(tmpDupeDF, !is.na(CreditScore))
  tmpDupeDF <- filter(tmpDupeDF, !CurrentLoanAmount == 99999999)
  myLoanData <- rbind(myLoanDataLoanID, tmpDupeDF)
  rm(tmpDupeDF, tmpDupeListLoanID, myLoanDataLoanID)

#3B:  
myLoanData$Bankruptcies<-as.integer(myLoanData$Bankruptcies)
myLoanData$TaxLiens<-as.integer(myLoanData$TaxLiens)

#4:  R Script
#require(data.table)
#myLoanData$LoanStatusNum<-ifelse(myLoanData$LoanStatus=="Charged Off",1,0)  #Why?

#Convert String to Numeric
myLoanData$MonthlyDebt<-sub(",","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-sub("\\$","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-as.numeric(myLoanData$MonthlyDebt)

#remove outliers
# myLoanData$MonthlyDebt[myLoanData$MonthlyDebt > 2299 | is.na(myLoanData$MonthlyDebt)] <- 2300
# summary(myLoanData$MonthlyDebt)

#Not using data table
myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit == "#VALUE!"] <- NA #2 NAs
myLoanData$MaximumOpenCredit = as.integer(myLoanData$MaximumOpenCredit)
myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit==0] <- 225
#myLoanData <- myLoanData[complete.cases(myLoanData$MaximumOpenCredit),] #####################################NEW!!!

# myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit >74999] <- 75000

myLoanData$CreditScore[myLoanData$CreditScore > 999 & !is.na(myLoanData$CreditScore)] <- myLoanData$CreditScore/10
#summary(myLoanData$CreditScore)

#myLoanData$CreditScore[is.na(myLoanData$CreditScore)] <- 0#Is this the right thing to do??  NO!
#Lets get the average CS for the records where the loan was paid off and the ave for Charged off & long term vs Short Term
#Filter myLoanData into new DF, change the NA the the right mean and then rbind back
tmpCSDF <- filter(myLoanData, is.na(CreditScore))
myLoanData <- filter(myLoanData, !is.na(CreditScore))

avePaidOff_CS_LT <-as.integer(myLoanData %>%  filter(LoanStatus=="Fully Paid", Term=="Long Term") %>% summarize(tmpMean = mean(CreditScore)))
avePaidOff_CS_ST <-as.integer(myLoanData %>%  filter(LoanStatus=="Fully Paid", Term=="Short Term") %>% summarize(tmpMean = mean(CreditScore)))
aveChargedOff_CS_LT <- as.integer(myLoanData %>%  filter(LoanStatus=="Charged Off", Term=="Long Term") %>% summarize(tmpMean = mean(CreditScore)))
aveChargedOff_CS_ST <- as.integer(myLoanData %>%  filter(LoanStatus=="Charged Off", Term=="Short Term") %>% summarize(tmpMean = mean(CreditScore)))

tmpCSDF$CreditScore[tmpCSDF$LoanStatus=="Fully Paid" & tmpCSDF$Term=="Long Term"] <- avePaidOff_CS_LT
tmpCSDF$CreditScore[tmpCSDF$LoanStatus=="Fully Paid" & tmpCSDF$Term=="Short Term"] <- avePaidOff_CS_ST
tmpCSDF$CreditScore[tmpCSDF$LoanStatus=="Charged Off" & tmpCSDF$Term=="Long Term"] <- aveChargedOff_CS_LT
tmpCSDF$CreditScore[tmpCSDF$LoanStatus=="Charged Off" & tmpCSDF$Term=="Short Term"] <- aveChargedOff_CS_ST

myLoanData <- rbind(myLoanData, tmpCSDF)
summary(myLoanData$CreditScore)
rm(tmpCSDF, aveChargedOff_CS_ST, aveChargedOff_CS_LT, avePaidOff_CS_ST, avePaidOff_CS_LT)

#Noted that certain Credit Scores has unusually high percentages
#691 CS = Charged Off; 701 CS = Fully Paid; 721 = Charged Off; CS = Fully Paid
myLoanData$SpecialCase[myLoanData$CreditScore == 691] <- 1
myLoanData$SpecialCase[myLoanData$CreditScore == 701] <- 2
myLoanData$SpecialCase[myLoanData$CreditScore == 721] <- 1
myLoanData$SpecialCase[myLoanData$CreditScore == 729] <- 2
myLoanData$SpecialCase[myLoanData$CreditScore == 740] <- 2
myLoanData$SpecialCase[myLoanData$CreditScore > 741] <- 2
myLoanData$SpecialCase[myLoanData$CurrentLoanAmount == 99999999] <- 2


# myLoanData$AnnualIncome[myLoanData$AnnualIncome > 138999] <- 139000 
myLoanData$AnnualIncome[myLoanData$AnnualIncome < 10001] <- 10000 
tmpmyLoanData <- myLoanData[!myLoanData$AnnualIncome > 2000000,]

myLoanData$AnnualIncome <- as.integer(myLoanData$AnnualIncome)
#summary(myLoanData$AnnualIncome) 16088 NAs

myLoanData$CurrentCreditBalance <- as.integer(myLoanData$CurrentCreditBalance)
# myLoanData$CurrentCreditBalance[myLoanData$CurrentCreditBalance > 29999] <- 30000 

myLoanData$NumberOfOpenAccounts <- as.integer(myLoanData$NumberOfOpenAccounts)
# myLoanData$NumberOfOpenAccounts[myLoanData$NumberOfOpenAccounts > 21] <- 22

myLoanData$YearsOfCreditHistory <- as.integer(myLoanData$YearsOfCreditHistory)
# myLoanData$YearsOfCreditHistory[myLoanData$YearsOfCreditHistory > 32] <- 33

#Strategy for MonthsSinceLastDelinquent
#Make NA = 99 - longer is better
#Add new column EverDelinquent 0 or 1
myLoanData$MonthsSinceLastDelinquent[is.na(myLoanData$MonthsSinceLastDelinquent)] <- 99
myLoanData$MonthsSinceLastDelinquent <-  ifelse(myLoanData$MonthlyDebt > 83, 1, ifelse(myLoanData$MonthlyDebt >12, 2, 3))
#myLoanData$EverDelinquent <- ifelse(myLoanData$MonthsSinceLastDelinquent == 99, 0, 1)

myLoanData$CurrentLoanAmount <- as.integer(myLoanData$CurrentLoanAmount)
#myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 0] <- 1 #Do I really want to do this?
myLoanData$SpecialCase[myLoanData$CurrentLoanAmount == 99999999] <- 2
#myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 99999999] <- 0#Use MICE?

summary(myLoanData$CurrentLoanAmount)#Big Difference

myLoanData$YearsInCurrentJob <- as.integer(gsub("([0-9]*).*","\\1",myLoanData$YearsInCurrentJob))
summary(myLoanData$YearsInCurrentJob)
#11,129 NAs.  Median - 7 Mean - 6.42. Replace with mean but need to ensure Median > Mean
myLoanData$YearsInCurrentJob[is.na(myLoanData$YearsInCurrentJob)] <- as.integer(1.1*mean(myLoanData$YearsInCurrentJob, na.rm = TRUE))
summary(myLoanData$YearsInCurrentJob)
# myLoanData$PaidInFull<-0
# myLoanData$ChargedOff<-0

#DT[,NumberOfLoansClean:=length(LoanID),by="CustomerID"]
#Big differecne.  I deleted the duplicate, this counts them as 2

#5B  Loan Purpose Factors

myLoanData$Purpose <- as.factor(myLoanData$Purpose)
myLoanData$Purpose[myLoanData$Purpose == "other"] <- "Other"

#Add a level
#Going to revamp - based on the counts for grouping
myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "BuyCarHouse"))
myLoanData$Purpose[myLoanData$Purpose == "Buy House"] <- "BuyCarHouse"
myLoanData$Purpose[myLoanData$Purpose == "Buy a Car"] <- "BuyCarHouse"

#myLoanData$Purpose[myLoanData$Purpose == "small_business"] <- "Business Loan"

myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "Misc"))
myLoanData$Purpose[myLoanData$Purpose == "major_purpose"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Educational Expenses"] <- "Misc"
#myLoanData$Purpose[myLoanData$Purpose == "Home Improvements"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "moving"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "renewable_energy"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "vacation"] <- "Misc"
#myLoanData$Purpose[myLoanData$Purpose == "Take a Trip"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "wedding"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Medical Bills"] <- "Misc"

myLoanData$Purpose <- droplevels(myLoanData$Purpose)

myLoanData$HomeOwnership[myLoanData$HomeOwnership == "HaveMortgage"] <- "Home Mortgage"
myLoanData$HomeOwnership <- as.factor(myLoanData$HomeOwnership)

#12
myLoanData$DisposableIncome <- myLoanData$AnnualIncome -((myLoanData$MonthlyDebt *12) - ifelse(myLoanData$HomeOwnership == "Own Home", 0, -(myLoanData$AnnualIncome * .15)))
####myLoanData$SpecialCase[myl$DisposableIncome > 150000]

myLoanData$DTI <- (myLoanData$MonthlyDebt * 12)/myLoanData$AnnualIncome#Fixed
myLoanData$AllCreditProbs <- myLoanData$NumberOfCreditProblems + myLoanData$Bankruptcies + myLoanData$TaxLiens + myLoanData$EverDelinquent
myLoanData$DisposableIncomePct <- myLoanData$DisposableIncome/myLoanData$AnnualIncome

myLoanData$PctCreditUsed <- myLoanData$CurrentCreditBalance / myLoanData$MaximumOpenCredit


myLoanData$CreditHistoryWeight <- myLoanData$CreditScore * myLoanData$YearsOfCreditHistory
myLoanData$LoanToMaxAvailableRatio <- ifelse(myLoanData$MaximumOpenCredit == 0 ,100, myLoanData$CurrentLoanAmount/ myLoanData$MaximumOpenCredit)
#myLoanData$TotalCreditProblems <- myLoanData$TaxLiens + myLoanData$Bankruptcies + myLoanData$NumberOfCreditProblems

#New Special Cases:  when LoanToMaxAvailableRatio = 0 (8249), 100% Fully Paid Disposable Income > .452
myLoanData$SpecialCase[myLoanData$LoanToMaxAvailableRatio ==0 & myLoanData$DisposableIncomePct > 0.452] <- 2

write.csv(myLoanData, "tempCSV1.csv")

####################################################################################

## Features to plot 
all.cols <- names(myLoanData)
all.cols

num.cols1 <- c("CurrentLoanAmount", "CreditScore", "YearsInCurrentJob", "AnnualIncome", "MonthlyDebt", "YearsOfCreditHistory", 
               "MonthsSinceLastDelinquent", "NumberOfOpenAccounts", "NumberOfCreditProblems", "CurrentCreditBalance", 
               "MaximumOpenCredit")
num.cols2 <- c("Bankruptcies", "TaxLiens", "DisposableIncome", "AllCreditProbs", "DisposableIncomePct", "PctCreditUsed", 
               "CreditHistoryWeight", "LoanToMaxAvailableRatio", "TotalCreditProblems", "DTI")

#pairs(~., data=myLoanData[sample(myLoanData, 8000), num.cols1])

#Conditioned histograms
plot.cols1 <- c("CurrentLoanAmount", "CreditScore", "YearsInCurrentJob", "AnnualIncome", "MonthlyDebt", "YearsOfCreditHistory", 
"DisposableIncome")
plot.cols2 <- c("DTI", "DisposableIncomePct", "PctCreditUsed", "CreditHistoryWeight", "LoanToMaxAvailableRatio", "TotalCreditProblems")

## Function to plot conditioned histograms
auto.hist <- function(x) {
    library(ggplot2)
    library(gridExtra)
    ## Compute the bin width
    rg = range(myLoanData[,x])
    bw = (rg[2] - rg[1])/30
    ## Define the title
    title <- paste("Histogram of", x, "conditioned on loan status")
    ## Create the histogram
    ggplot(myLoanData, aes_string(x)) +
      geom_histogram(aes(y = ..count..), binwidth = bw) +
      facet_grid(. ~ LoanStatus) +
      ggtitle(title) 
  }

lapply(plot.cols2, auto.hist)

## Function to create conditioned box plots
auto.box <- function(x) {
  title <- paste("Box plot of", x, "by loan status")
  ggplot(myLoanData, aes_string("LoanStatus", x)) +
    geom_boxplot() + ggtitle(title)
}
lapply(num.cols2, auto.box)

## Scatter plot using color to differentiate points
auto.scatter <- function(x){
  require(ggplot2)
  title <- paste("CreditScore vs.", x, "with color by LoanStatus")
  ggplot(myLoanData, aes_string("CreditScore", x)) +
    geom_point(aes(color = factor(LoanStatus))) +
    ggtitle(title)
}
lapply(plot.cols1, auto.scatter)

## Conditioned scatter plots 
auto.scatter.cond <- function(x){   
  require(ggplot2)   
  library(gridExtra)   
  title <- paste("price vs.", x, 'with color by credit score and DTI') 
  ggplot(myLoanData, aes_string("CreditScore", x)) + 
    geom_point(aes(color = factor(LoanStatus))) +
    facet_grid(CreditScore ~ DTI) + ggtitle(title)
  }

auto.scatter.cond(num.cols1)

## Bar plot of categorical features 
bar.categorical <- function(x){   
  library(ggplot2)   
  if(!is.numeric(myLoanData[sample(2000),x])){     
    capture.output(       
      plot( ggplot(myLoanData, aes_string(x)) + geom_bar() + facet_grid(. ~ LoanStatus) +
              ggtitle(paste("Counts of Loan Status level by",x))))
  }
}

lapply(all.cols[-c(1:3)], bar.categorical)

## Create Box plot of numeric features  
box.numeric <- function(x){
  library(ggplot2)   
  if(is.numeric(myLoanData[sample(2000),x])){     
    capture.output(
      plot(ggplot(myLoanData, aes_string('LoanStatus', x)) + geom_boxplot() + 
              ggtitle(paste("Counts of Loan Status by",x))))   
     }#The capture.output function is used to capture and suppress voluminous 
      #output from ggplot. 
  } 
lapply(all.cols[-c(1:3)], box.numeric)
