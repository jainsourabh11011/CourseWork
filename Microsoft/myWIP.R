
#1 Load Data
myLoanData <- read.csv("LoansTrainingSet.csv", header=TRUE, stringsAsFactors = FALSE)

#2 Remove obvious duplicates
myLoanData<- myLoanData[!duplicated(myLoanData),]

#3A R Script - Column Names
columnnames<-c("LoanID","CustomerID","LoanStatus","CurrentLoanAmount","Term","CreditScore","YearsInCurrentJob","HomeOwnership","AnnualIncome","Purpose","MonthlyDebt","YearsOfCreditHistory","MonthsSinceLastDelinquent","NumberOfOpenAccounts","NumberOfCreditProblems","CurrentCreditBalance","MaximumOpenCredit","Bankruptcies","TaxLiens")
colnames(myLoanData)<-columnnames

#3B:  
myLoanData$Bankruptcies<-as.integer(myLoanData$Bankruptcies)
myLoanData$TaxLiens<-as.integer(myLoanData$TaxLiens)
myLoanData$TermClean<-myLoanData$Term

#4:  R Script

#require(data.table)

myLoanData$LoanStatusNum<-ifelse(myLoanData$LoanStatus=="Charged Off",1,0)  #Why?

#Convert String to Numeric
myLoanData$MonthlyDebt<-sub(",","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-sub("\\$","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-as.numeric(myLoanData$MonthlyDebt)

#remove outliers
myLoanData$MonthlyDebt[myLoanData$MonthlyDebt > 2299 | is.na(myLoanData$MonthlyDebt)] <- 2300
summary(myLoanData$MonthlyDebt)

#Not using data table
myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit == "#VALUE!"] <- NA #2 NAs
myLoanData$MaximumOpenCredit = as.integer(myLoanData$MaximumOpenCredit)
#myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit >74999] <- 75000
myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit >124999] <- 125000

myLoanData$AnnualIncome <- as.integer(myLoanData$AnnualIncome)
#myLoanData$AnnualIncome[myLoanData$AnnualIncome > 138999] <- 139000 
myLoanData$AnnualIncome[myLoanData$AnnualIncome > 199999] <- 200000 
myLoanData$AnnualIncome[myLoanData$AnnualIncome < 10001] <- 10000 

#Using my own credit score code
myLoanData$CreditScore[is.na(myLoanData$CreditScore)] <- 0
myLoanData$CreditScore[myLoanData$CreditScore > 999] <- myLoanData$CreditScore/10
summary(myLoanData$CreditScore)

myLoanData$CurrentCreditBalance <- as.integer(myLoanData$CurrentCreditBalance)
myLoanData$CurrentCreditBalance[myLoanData$CurrentCreditBalance > 29999] <- 30000 

myLoanData$NumberOfOpenAccounts <- as.integer(myLoanData$NumberOfOpenAccounts)
#myLoanData$NumberOfOpenAccounts[myLoanData$NumberOfOpenAccounts > 21] <- 22
myLoanData$NumberOfOpenAccounts[myLoanData$NumberOfOpenAccounts > 14] <- 15

myLoanData$YearsOfCreditHistory <- as.integer(myLoanData$YearsOfCreditHistory)
#myLoanData$YearsOfCreditHistory[myLoanData$YearsOfCreditHistory > 32] <- 33
myLoanData$YearsOfCreditHistory[myLoanData$YearsOfCreditHistory > 19] <- 20

myLoanData$CurrentLoanAmount <- as.integer(myLoanData$CurrentLoanAmount)
myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 0] <- 1
summary(myLoanData$CurrentLoanAmount)#Big Difference
myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 99999999] <- 1#Try MICE?

myLoanData$YearsInCurrentJob <- as.integer(gsub("([0-9]*).*","\\1",myLoanData$YearsInCurrentJob))
myLoanData$PaidInFull<-0
myLoanData$ChargedOff<-0


#DT[,NumberOfLoansClean:=length(LoanID),by="CustomerID"]
#Big differecne.  I deleted the duplicate, this counts them as 2

#5B  Loan Purpose Factors

myLoanData$Purpose <- as.factor(myLoanData$Purpose)
myLoanData$Purpose[myLoanData$Purpose == "other"] <- "Other"

#Add a level
myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "BuyCarHouse"))
myLoanData$Purpose[myLoanData$Purpose == "Buy House"] <- "BuyCarHouse"
myLoanData$Purpose[myLoanData$Purpose == "Buy a Car"] <- "BuyCarHouse"

myLoanData$Purpose[myLoanData$Purpose == "small_business"] <- "Business Loan"

myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "Misc"))
myLoanData$Purpose[myLoanData$Purpose == "major_purpose"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Educational Expenses"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Home Improvements"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "major_purchase"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "moving"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "renewable_energy"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "vacation"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Take a Trip"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "wedding"] <- "Misc"
myLoanData$Purpose[myLoanData$Purpose == "Medical Bills"] <- "Misc"

myLoanData$Purpose <- droplevels(myLoanData$Purpose)


#12
myLoanData$DisposableIncome <- myLoanData$AnnualIncome -((myLoanData$MonthlyDebt *12) - ifelse(myLoanData$HomeOwnership == "Own Home", 0, -(myLoanData$AnnualIncome * .15)))

myLoanData$DTI <- (myLoanData$MonthlyDebt * 12)/myLoanData$AnnualIncome#FIXED
myLoanData$AllCreditProbs <- myLoanData$NumberOfCreditProblems + myLoanData$Bankruptcies + myLoanData$TaxLiens
myLoanData$DisposableIncomePct <- myLoanData$DisposableIncome/myLoanData$AnnualIncome
myLoanData$PctCreditUsed <- ifelse(myLoanData$MaximumOpenCredit==0 ,max(100,myLoanData$MaximumOpenCredit), myLoanData$CurrentCreditBalance / myLoanData$MaximumOpenCredit)
myLoanData$CreditHistoryWeight <- myLoanData$CreditScore * myLoanData$YearsOfCreditHistory
myLoanData$LoanToMaxAvailableRatio <- ifelse(myLoanData$MaximumOpenCredit == 0 ,100, myLoanData$CurrentLoanAmount/ myLoanData$MaximumOpenCredit)
myLoanData$TotalCreditProblems <- myLoanData$TaxLiens + myLoanData$Bankruptcies + myLoanData$NumberOfCreditProblems


################################################################################
library(dplyr)


tmpDupeList <- unique(myLoanData[duplicated(myLoanData$CustomerID),])#Returns the individual(only 1 record) duplicate Cust_IDs
tmpDupeListCustomerID <- tmpDupeList[,2]#Provides the list of Cust_IDs that have duplicates
rm(tmpDupeList)
#Subset master with the values in the duplicated Cust_ID DF
tmpDupeDF <- filter(myLoanData, CustomerID %in% tmpDupeListCustomerID)#Got all the dupes in one DF, now remove them from master file
myLoanData <- filter(myLoanData, !CustomerID %in% tmpDupeListCustomerID)#All unique Cust_IDs YEAH!!  Will RBIND later
rm(tmpDupeListCustomerID)
#Now work on the duplcate file
tmpDupeDF <- arrange(tmpDupeDF, desc(CustomerID))

tmpDupeDF <- filter(tmpDupeDF, !is.na(CreditScore))
tmpDupeDF <- filter(tmpDupeDF, !CurrentLoanAmount == 99999999)
myLoanData <- rbind(myLoanData, tmpDupeDF)
rm(tmpDupeDF)

##################

#library(stringr)
#Clean Yrs_in_Job - n/a have hi percent Fully Paid so keep them
#Create new descriptive factor
#Below I added prefix of 11 so if I want to extract the numbers and treat as numeric, I can
myLoanData$Yrs_in_Job <- factor(myLoanData$Yrs_in_Job, 
                                levels = c(levels(myLoanData$Yrs_in_Job), "11HiFullyPaidPercent"))
#Put n/a into new factor level
myLoanData$Yrs_in_Job[myLoanData$Yrs_in_Job == "n/a"] <- "11HiFullyPaidPercent"
#Drop n/a label
myLoanData$Yrs_in_Job <- droplevels(myLoanData$Yrs_in_Job)
#Reorder levels
myLoanData$Yrs_in_Job <- factor(myLoanData$Yrs_in_Job, levels(myLoanData$Yrs_in_Job)[c(1, 2, 4:11, 3, 12)])
#Extract the integers from the Yrs_in_Job string
#Decided to keep it as a label
#myLoanData$NewYrs_in_Job <- str_extract(test$Yrs_in_Job, "[0-9]+")

##########################

myLoanData <- maml.mapInputPort(1) # class: data.frame
#library(dplyr)
#print(str(myLoanData))
myLoanData$Yrs_Credit_Hist2 <- cut(myLoanData$Yrs_Credit_Hist, c(0, 5, 10, 15, 20, 25, 30, 40, 100), 
                                   labels = c(5, 10, 15, 20, 25, 30, 40, 50), ordered_result = TRUE)

#print(str(myLoanData))
# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("myLoanData");

###############################

library(dplyr)
#There are $ in a few records - remove them
#First find them:
tmpCHAR <- grep("$", myLoanData$Monthly_Debt, fixed=TRUE)
for(i in 1:length(tmpCHAR)){
  myLoanData[tmpCHAR[i],11] <- substr(myLoanData[tmpCHAR[i],11], 2, 
                                      nchar(myLoanData[tmpCHAR[i],11]))
}
myLoanData$Monthly_Debt <- as.numeric(myLoanData$Monthly_Debt)

#Need to solve all NAs - assuming NA is equivalent to NONE
myLoanData$Cnt_Bankruptcy[myLoanData$Cnt_Bankruptcy == "NA"] <- "0"
myLoanData$Cnt_Bankruptcy <- as.integer(myLoanData$Cnt_Bankruptcy)

myLoanData$Months_Last_Delinq[myLoanData$Months_Last_Delinq == "NA"] <- "0"#NA is actually a string in the data
myLoanData$Months_Last_Delinq <- as.integer(myLoanData$Months_Last_Delinq)

#There is one entry of #VALUE!:  Cust_ID = a679ed55-963a-4de6-8be0-4364eb601b6f
myLoanData <- filter(myLoanData, Cust_ID != "a679ed55-963a-4de6-8be0-4364eb601b6f")
myLoanData$Max_Open_Credit <- as.integer(myLoanData$Max_Open_Credit)

myLoanData$Cnt_Tax_Liens[myLoanData$Cnt_Tax_Liens == "NA"] <- "0"
myLoanData$Cnt_Tax_Liens <- as.integer(myLoanData$Cnt_Tax_Liens)

#########################

myLoanData <- maml.mapInputPort(1) # class: data.frame
library(dplyr)
#There are $ in a few records - remove them
#First find them:
tmpCHAR <- grep("$", myLoanData$Monthly_Debt, fixed=TRUE)
for(i in 1:length(tmpCHAR)){
  myLoanData[tmpCHAR[i],11] <- substr(myLoanData[tmpCHAR[i],11], 2, 
                                      nchar(myLoanData[tmpCHAR[i],11]))
}
myLoanData$Monthly_Debt <- as.numeric(myLoanData$Monthly_Debt)

#Need to solve all NAs - assuming NA is equivalent to NONE
myLoanData$Cnt_Bankruptcy[myLoanData$Cnt_Bankruptcy == "NA"] <- "0"
myLoanData$Cnt_Bankruptcy <- as.integer(myLoanData$Cnt_Bankruptcy)

myLoanData$Months_Last_Delinq[myLoanData$Months_Last_Delinq == "NA"] <- "0"#NA is actually a string in the data
myLoanData$Months_Last_Delinq <- as.integer(myLoanData$Months_Last_Delinq)

#There is one entry of #VALUE!:  Cust_ID = a679ed55-963a-4de6-8be0-4364eb601b6f
myLoanData <- filter(myLoanData, Cust_ID != "a679ed55-963a-4de6-8be0-4364eb601b6f")
myLoanData$Max_Open_Credit <- as.integer(myLoanData$Max_Open_Credit)

myLoanData$Cnt_Tax_Liens[myLoanData$Cnt_Tax_Liens == "NA"] <- "0"
myLoanData$Cnt_Tax_Liens <- as.integer(myLoanData$Cnt_Tax_Liens)
