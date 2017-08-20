
#1 Load Data
dat <- read.csv("LoansTrainingSet.csv", header=TRUE, stringsAsFactors = FALSE)

#2 Remove Duplicates
dat<- dat[!duplicated(dat),]

#3 R Script
columnnames<-c("LoanID","CustomerID","LoanStatus","CurrentLoanAmount","Term","CreditScore","YearsInCurrentJob","HomeOwnership","AnnualIncome","Purpose","MonthlyDebt","YearsOfCreditHistory","MonthsSinceLastDelinquent","NumberOfOpenAccounts","NumberOfCreditProblems","CurrentCreditBalance","MaximumOpenCredit","Bankruptcies","TaxLiens")
colnames(dat)<-columnnames

dat$MonthsSinceLastDelinquent<-as.integer(dat$MonthsSinceLastDelinquent)
dat$Bankruptcies<-as.integer(dat$Bankruptcies)
dat$TaxLiens<-as.integer(dat$TaxLiens)
dat$TermClean<-dat$Term

#4 R Script
require(data.table)

dat$LoanStatusNum<-ifelse(dat$LoanStatus=="Charged Off",1,0)

#Convert String to Numeric
dat$MonthlyDebt<-sub(",","",as.character(dat$MonthlyDebt))
dat$MonthlyDebt<-sub("\\$","",as.character(dat$MonthlyDebt))
dat$MonthlyDebt<-as.numeric(dat$MonthlyDebt)

#remove outliers
dat.md<-ifelse(dat$MonthlyDebt<2300 |is.na(dat$MonthlyDebt),dat$MonthlyDebt,2300)
#How would anyone know about $2300.  it seems low to me.

dat$MonthlyDebt<-dat.md
rm(dat.md)
summary(dat$MonthlyDebt)

DT<-data.table(dat)
setkey(DT,LoanID,CustomerID)

DT[,MaximumOpenCredit:=(MaximumOpenCredit)]
DT$MaximumOpenCredit<ifelse(DT$MaximumOpenCredit=="#Value!",NA,DT$MaximumOpenCredit)
DT[,MaximumOpenCredit:=as.integer(MaximumOpenCredit)]
DT[,MaximumOpenCreditClean:=MaximumOpenCredit]
DT$MaximumOpenCreditClean<-as.integer(ifelse(is.na(DT$MaximumOpenCredit),-1,DT$MaximumOpenCredit))
DT[,MaximumOpenCreditClean:=as.integer(max(MaximumOpenCreditClean),na.rm=TRUE),by="LoanID,CustomerID"]
DT[,MaximumOpenCreditClean:=as.integer(max(MaximumOpenCreditClean),na.rm=TRUE),by="CustomerID"]
DT$MaximumOpenCreditClean<-as.integer(ifelse(DT$MaximumOpenCreditClean==-1,NA,ifelse(DT$MaximumOpenCreditClean>75000,75000,DT$MaximumOpenCreditClean)))
#Where does the 75000 limit come from?

DT$MaximumOpenCreditClean<-as.integer(ifelse(DT$MaximumOpenCreditClean<0,0,DT$MaximumOpenCreditClean))

DT[,AnnualIncome:=as.integer(AnnualIncome)]
DT[,AnnualIncomeClean:=AnnualIncome]
DT$AnnualIncomeClean<-as.integer(ifelse(is.na(DT$AnnualIncome),-1,DT$AnnualIncome))
DT[,AnnualIncomeClean:=as.integer(max(AnnualIncomeClean),na.rm=TRUE),by="LoanID,CustomerID"]
DT[,AnnualIncomeClean:=as.integer(max(AnnualIncomeClean),na.rm=TRUE),by="CustomerID"]

DT$AnnualIncomeClean<-as.integer(ifelse(DT$AnnualIncomeClean==-1,NA,ifelse(DT$AnnualIncomeClean>139000,139000,ifelse(DT$AnnualIncomeClean<10000,10000,DT$AnnualIncomeClean))))
#How did the min and max get determined?


DT[,CreditScore:=as.integer(CreditScore)]
DT[,CreditScoreClean:=CreditScore]

DT$CreditScoreClean<-as.integer(ifelse(is.na(DT$CreditScore),-1,ifelse(DT$CreditScore>801,600,ifelse(DT$CreditScore<600,600,DT$CreditScore))))
#Did not divide the large ones like i did.  Also set a 600 min.
DT[,CreditScoreClean:=as.integer(max(CreditScoreClean),na.rm=TRUE),by="LoanID,CustomerID"]
DT[,CreditScoreClean:=as.integer(max(CreditScoreClean),na.rm=TRUE),by="CustomerID"]
DT$CreditScoreClean<-as.integer(ifelse(DT$CreditScoreClean==-1,NA,DT$CreditScoreClean))
summary(DT$CreditScoreClean)

DT$CurrentCreditBalance<-as.integer(ifelse(DT$CurrentCreditBalance==0,1,ifelse(is.na(DT$CurrentCreditBalance)|DT$CurrentCreditBalance<=30000,DT$CurrentCreditBalance,30000)))
#Where did a min 30000 come from?  Set 0 to 1.
DT$NumberOfOpenAccounts<-as.integer(ifelse(is.na(DT$NumberOfOpenAccounts)|DT$NumberOfOpenAccounts<=22,DT$NumberOfOpenAccounts,22))
#Where does 22 as a max come from?  
DT$YearsOfCreditHistory<-as.integer(ifelse(is.na(DT$YearsOfCreditHistory)|DT$YearsOfCreditHistory<=33,DT$YearsOfCreditHistory,33))
#Why 33 years as max?
DT[,CurrentLoanAmountClean:=as.integer(min(CurrentLoanAmount,na.rm=TRUE)),by="LoanID,CustomerID"]
DT[,CurrentLoanAmountClean:=ifelse(CurrentLoanAmountClean==0,1,CurrentLoanAmountClean)]
summary(DT$CurrentLoanAmountClean)
#Sets 0 to 1

#Initialize for SQL next step
DT$YearsInCurrentJobClean<-as.integer(gsub("([0-9]*).*","\\1",DT$YearsInCurrentJob)) #extract numbers 
DT$PaidInFull<-0
DT$ChargedOff<-0
DT[,NumberOfLoansClean:=length(LoanID),by="CustomerID"]
#Big differecne.  I deleted the duplicate, this counts them as 2.  I Think this is wrong


#5  Apply SQL Transformation
# UPDATE t1 SET 
# [PaidInFull] = CASE 
# WHEN [CurrentLoanAmountClean] = 99999999 THEN 1 
# -- WHEN CreditScore >800 THEN -1
# ELSE 0 END,
# [CurrentLoanAmountClean] = CASE 
# WHEN [CurrentLoanAmountClean] = 99999999 THEN 'NA' 
# WHEN CurrentLoanAmountClean IS NULL THEN 'NA' 
# WHEN CurrentLoanAmountClean <=0 THEN 1
# ELSE [CurrentLoanAmountClean] END,
# 
# [ChargedOff] = CASE WHEN CreditScore>800 THEN 1 ELSE 0 END,
# [HomeOwnership] = CASE WHEN [HomeOwnership] = 'HaveMortgage' THEN 'Home Mortgage' ELSE [HomeOwnership] END,
# [Purpose] = CASE WHEN [Purpose] = 'other' THEN 'Other' ELSE [Purpose] END; 
# 
# SELECt * FROM t1;

#6
#Exclude:  CurrentLoanAmount, CreditScore, MaximumOpenCredit, MonthsSinceLastDleinquent, YearsInCurrentJobClean

#7 Remove Duplicate Rows
#Remove Duplicate LoanID

#8 Execute R Script
DT<-data.table(dat)
setkey(DT,LoanID,CustomerID)

DT[,NumberOfLoansClean:=length(LoanID),by="CustomerID"]
DT[,HomeOwnershipClean:=HomeOwnership]
DT[,HomeOwnershipCatClean:=ifelse(HomeOwnership=="Own Home","Own","Paying")]

#9 Clean Missing Data
#CurrentCreditBalance, YearsOfCreditHistory, CreditScoreClean
#Used MICE

#10 Clean Missing Values
#AnnualIncomeClean,MonthlyDebt,MaximumOpenCreditClean,CurrentLoanAmountClean, 
#NumberOfCreditProblems,CreditScoreClean,YearsInCurrentJobClean,NumberOfLoansClean
#used MICE

#11 Clean Missing Data
#NumberOfCreditProblems,TaxLiens,Bankruptcies
#Custom Substitution Value - 0

#12 Execute R Script
library(data.table)
DT<-data.table(dat)
setkey(DT,LoanID,CustomerID)

DT$DisposableIncome<-DT$AnnualIncomeClean - ((DT$MonthlyDebt * 12) - ifelse(DT$HomeOwnership=="Own Home",0,-(DT$AnnualIncomeClean * .15)))
#Curious adjustment for people that do not onwn home.
DT$DisposableIncome<-ifelse(DT$DisposableIncome<0,1,DT$DisposableIncome)
#Seems like a good idea
DT$DisposableIncomePct<-DT$DisposableIncome/DT$AnnualIncomeClean
#Seems like an OK idea
DT$PctCreditUsed<-ifelse(DT$MaximumOpenCreditClean==0 ,max(100,DT$MaximumOpenCreditClean), DT$CurrentCreditBalance / DT$MaximumOpenCreditClean)
#Seems like an OK idea
DT$CreditHistoryWeight<-DT$CreditScoreClean * DT$YearsOfCreditHistory
#Interesting
DT$DebtToIncomeRatio<-((DT$MonthlyDebt * 12))/DT$AnnualIncomeClean
#Yeap.
DT$LoanToMaxAvailableRatio<-ifelse(DT$MaximumOpenCreditClean==0 ,100, DT$CurrentLoanAmountClean/ DT$MaximumOpenCreditClean)
#Hmm not sure
DT$TotalCreditProblems<-DT$TaxLiens + DT$Bankruptcies + DT$NumberOfCreditProblems
#Yeap

#13 Apply Math Operation
#Ln CreditScoreClean - Append


#14 #Edit MetaData
#Term,HomeOwnership,Purpose,HomeOwnershipCatClean,LoanStatus,LoanStatusNum,LoanStatusNum,PaidInFull,ChargedOff
#Make categorical

#15 Split Data
#.75

#16 Group Data into Bins
#LoanStatus:  CreditScoreClean,CurrentLoanAmountClean,DisposableIncome,AnnualIncomeClean, DisposableIncomePct,PctCreditUsed
#Entropy MDL, 10 Bins, Append, Tag columns as categorical
#Why not bin bedfore the split?

#17 Build counting transform
# 2 classes, 20 bit, 1 seed, Dataset, Dictionary, LoanStatus:  Purpose,PaidInFull,ChargedOff,HomeOwnershipClean,TermClean

#18 Modify Count Table Parameters
#Garbage 10, 42 prior, noise 0, LogOddsOnly, Ignore back off column

#19 Apply Transform

#20 Select Cols in Dataset
#LoanStatus,LoanID,CreditScoreClean_quantized,ChargedOff - Class000_LogOdds,CreditHistoryWeight,
#PaidInFull - Class000_LogOdds,TermClean - Class000_LogOdds,CurrentLoanAmountClean_quantized,DisposableIncome_quantized,
#DebtToIncomeRatio,AnnualIncomeClean_quantized,PctCreditUsed_quantized

#21 Tune Hyperparameters
#Randow Sweep, 5 runs, 0 seed, LoanStatus, Accuracy, Mean Absolute Error

#20.5 Two Class Boosted Tree
#20 leaves, 10 samples, 0.2 Learning, 100 tress, Allow unknown catagorical levels