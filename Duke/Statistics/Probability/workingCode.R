
if(!file.exists("./data/StormData.csv")){
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
  download.file(fileUrl, destfile="./data/StormData.csv.bz2", method="auto")
  gunzip("./data/StormData.csv.bz2",destname="./data/StormData.csv")
  fileUrl <- NULL
}




setwd("~/GitHub/IntroToProbability/project")#set the working directory
if(!exists("brfss2013")) load("./data/brfss2013.RData")#only load the data if it is not already loaded

#Before assigning a new column for US State v Not US State, let's make sure the state data looks clean:
unique(brfss2013$X_state)

library(dplyr)#needed to use the filter function below
modData <- filter(brfss2013, !X_state == "80"); modData <- filter(modData, !X_state == "0")
rm(brfss2013)#lets remove this to save on memory


modData <- modData %>% mutate(is_State = 1)#label all records as a state
modData$is_State[modData$X_state=="Guam"] <-3
modData$is_State[modData$X_state=="Puerto Rico"] <-2

library(ggplot2)
library(scales)
countNAs <- apply(modData, 2, function(x) sum(is.na(x)))
countNAs_df <- as.data.frame(countNAs)#change to data frame
countNAs_df$cat <- row.names(countNAs_df)#copy row names to its own column (variable)
options(scipen=999)#prevents scientific notation
countNAs_df$percentNA <- format(countNAs_df$countNAs/nrow(modData), digits = 2)
countNAs_df$percentNA <- as.numeric(countNAs_df$percentNA)

plot5 <- ggplot(countNAs_df, aes(x = reorder(cat, -countNAs), y = countNAs), 
                ylab("NA Count"), xlab("Variable Name")) +
  geom_bar(stat = "identity") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(labels = comma) + 
  xlab("Variable Names") + ylab("NA Count") + ggtitle("Count of NAs in Data") + coord_flip()
plot5

#Will set a varible to serve as a cuttoff of data based on the pecent of NAs
#cutoffValue = as.numeric(.3)
#varibleNames <-countNAs_df %>% filter(percentNA<cutoffValue)

#Lets save the varible names to use them to filter only the data we want

variablesIwant <- c("X_state","is_State", "addepev2", "alcday5", "asthma3", "beanday_", "bloodcho", "bphigh4", "chccopd1", "chckidny", "chcocncr", "chcscncr", "checkup1", "children","cholchk",  "cvdcrhd4", "cvdinfr4", "cvdstrk3", "decide", "diffalon","diffdres", "diffwalk", "doctdiab", "drnkany5", "drocdy3_", "educa", "employ1", "exerany2", "fc60_", "flushot6", "fruit1", "fruitju1", "frutda1_", "ftjuda1_", "fvbeans", "fvgreen", "fvorang", "genhlth", "grenday_", "havarth3", "height3","hivtst6", "hlthcvrg", "hlthpln1","income2", "is_State", "internet", "iyear", "marital", "medbills", "medcost", "medscost", "menthlth", "numadult", "nummen", "numwomen", "orngday_", "pa1vigm_", "persdoc2", "physhlth", "pneuvac3", "pvtresd1", "qlactlm2", "qstlang", "renthom1", "seatbelt", "sex", "sleptim1", "smoke100", "tetanus", "toldhi2", "useequip", "usenow3", "vegeda1_", "vegetab1", "veteran3", "weight2", "X_age80", "X_aidtst3", "X_asthms1", "X_casthm1", "X_chldcnt", "X_cholchk", "X_drdxar1", "X_drnkdy4", "X_drnkmo4",  "X_educag", "X_frtlt1", "X_frutsum", "X_imprace", "X_incomg", "X_lmtact1","X_lmtscl1", "X_lmtwrk1", "X_ltasth1", "X_misfrtn", "X_misvegn", "X_pa150r2", "X_pa30021", "X_pa300r2", "X_pacat1", "X_pastae1", "X_rfbing5",  "X_rfbmi5", "X_rfchol", "X_rfdrhv4", "X_rfhlth", "X_rfhype5", "X_rfseat2", "X_rfseat3", "X_rfsmok3", "X_smoker3", "X_totinda", "X_veg23", "X_vegesum", "X_veglt1")

workingData <- select(modData,one_of(variablesIwant))#new option for me in dplyr - useful

#Rename with dplyr
modData <- rename(workingData,  
  US_State = X_state,
  Is_US_State = is_State,
  Year = iyear, 
  AlcoholDays_Past_30 = alcday5, 
  Heavy_Alcohol_Calc = X_rfdrhv4,
  Beans_Day = beanday_, 
  Cholesteral_Check = bloodcho, 
  Last_Cholesteral_Chk = cholchk, 
  Number_Adults = numadult, 
  Number_Men = nummen, 
  Number_Women = numwomen, 
  General_Health = genhlth, 
  Phys_Not_OK =  physhlth, 
  Mental_Not_OK = menthlth, 
  Sleep_Hrs = sleptim1, 
  Depress_Disorder = addepev2, 
  Education = educa, 
  Weight = weight2, 
  Exercise_Past30 = exerany2, 
  Asthma = asthma3, 
  High_BP = bphigh4, 
  COPD = chccopd1, 
  Kidney_Disease = chckidny, 
  Other_Cancer = chcocncr, 
  Skin_Cancer = chcscncr, 
  Last_Checkup = checkup1, 
  Concentration = decide, 
  No_Children = children, 
  Heart_Disease = cvdcrhd4, 
  Heart_Attack = cvdinfr4, 
  Stroke = cvdstrk3, 
  Difficulty_Errands = diffalon, 
  Difficult_Dressing = diffdres, 
  Difficult_Walk = diffwalk, 
  Alcohol_Past_30 = drnkany5, 
  Computed_Drinks_Day = drocdy3_, 
  Doc_Visits_Diabetes = doctdiab, 
  Employment = employ1, 
  Functional_Cap = fc60_, 
  Arthritis = havarth3, 
  Flu_Shot_12M = flushot6, 
  Times_Eat_Fruit = fruit1, 
  Drink_Fruit_Juice = fruitju1, 
  Computed_Fruit_Day = frutda1_, 
  Fruit_Juice_Times_Day = ftjuda1_, 
  Times_EatBeans = fvbeans, 
  Times_Eat_Greens = fvgreen, 
  Times_Eat_Orange = fvorang, 
  Computed_OrangePer_Day = orngday_, 
  Computed_GreensIn_Day = grenday_, 
  Height = height3, 
  HIV_Tested = hivtst6, 
  Health_Ins = hlthcvrg, 
  Any_Health_Cov = hlthpln1, 
  Income = income2, 
  Internet_Past_30 = internet, 
  Marital_Status = marital, 
  Have_Med_Bills =  medbills, 
  Dr_Cost_Too_Much = medcost, 
  Meds_Too_Expensive = medscost, 
  Vigorous_Activity = pa1vigm_, 
  Multiple_Docs = persdoc2, 
  Pneumonioa_Ever =  pneuvac3, 
  Private_Home = pvtresd1, 
  Activity_Lim_dBy_Health = qlactlm2, 
  Language_ID = qstlang, 
  Own_Rent  = renthom1, 
  Freq_Use_Seatbelt = seatbelt, 
  Sex = sex, 
  Smoke_100 = smoke100, 
  Tentanus_2005 = tetanus, 
  Told_Hi_Cholesterol = toldhi2, 
  Health_Sp_Equip = useequip, 
  Smokeless_Tabacco = usenow3, 
  Computed_Veg_Per_Day = vegeda1_, 
  Verteran = veteran3, 
  Weight = weight2, 
  Age_Group_80 = X_age80, 
  Asthma_Status = X_asthms1, 
  Diagnosed_Arthritis = X_drdxar1, 
  Computed_Drinks_Per_Day = X_drnkdy4, 
  Drinks_Per_Month = X_drnkmo4, 
  Computed_Educ = X_educag, 
  Imputed_Ethnicity = X_imprace, 
  Adults_Good_Health = X_rfhlth,
  AIDS_Tested = X_aidtst3,
  Asthma_Calc_Variable = X_casthm1,
  Children_Calc = X_chldcnt,
  Cholesterol_Calc = X_cholchk,
  Fruit_1orMore_Day = X_frtlt1,
  Fruit_Total_Day = X_frutsum,
  Income_Calc = X_incomg,
  Limited_Usual_Act = X_lmtact1,
  Limited_Social_Act = X_lmtscl1,
  Limited_Work_Act = X_lmtwrk1)
```

countByState <- workingData %>% group_by(US_State) %>% summarize(RecordCount = n())
range(countByState$RecordCount)
filter(countByState, RecordCount == 33668)
filter(countByState, RecordCount == 1897)
#Cleanup evironmnet vaibles no longer needed
rm(countNAs_df); rm(countNAs); rm(modData); rm(variablesIwant); rm(cutoffValue)
rm(varibleNames); rm(countByState)

###Research Quesion 1
#Do people that use the Internet drink more than those that do not use the Internet

  # | M**y Variable Name** | **Original Variable Name** | **Description**
  # | ---------------- | ---------------------- | ----------------------------------------- |
  # | AlcoholDays_Past_30 | alcday5 | Days In Past 30 Had Alcoholic Beverage |
  # | Alcohol_Past_30 | drnkany5 | Drink Any Alcoholic Beverages In Past 30 Days |
  # | Computed_Drinks_Day | drocdy3_ | Computed Drink-Occasions-Per-Day |
  # | Number_Men | nummen | Number Of Adult Men In Household |
  # | Number_Women | numwomen | Number Of Adult Women In Household |
  # | Children_Calc | X_chldcnt | Computed Number Of Children In Household |
  # | No_Children | children | Number Of Children In Household |
  # | Internet_Past_30 | internet | Internet Use In The Past 30 Days? |
  # | Heavy_Alcohol_Calc | X_rfdrhv4 | Heavy Alcohol Consumption Calculated Variable |
  # | Is_US_State | NA- | 0 = Guam or Puerto Rico; 1 = US State |
  # 

variblesQuest1 <- c("internet", "X_rfdrhv4", "is_State", "sex")
quest1Data <- select(modData,one_of(variblesQuest1))
quest1Data <- rename(quest1Data, Heavy_Drinker = X_rfdrhv4)

#quest1Data2 <- filter(quest1Data, complete.cases(quest1Data))
#rm(quest1Data)

# - AlcoholDays_Past_30 - During the past 30 days, how many days per week or per month did you have at least one drink of any alcoholic beverage such as beer, wine, a malt beverage or liquor?  The first digit denotes days per week (1) or days per month (2). The remaining digits indicate the count of days. 
# + value range:  `range(quest1Data2$AlcoholDays_Past_30)`
# - Alcohol_Past_30 - Adults who reported having had at least one drink of alcohol in the past 30 days.  1 - Yes; 2 - No
# + value range:  `range(quest1Data2$Alcohol_Past_30)`
# - Computed_Drinks_Day - Calculated drink-occasions-per-day.  0 - No Drink-Occasions per day; 1 - many
# + value range:  `range(quest1Data2$Computed_Drinks_Day)`
# - Heavy_Alcohol_Calc - Heavy drinkers (adult men having more than two drinks per day and adult women having more than one drink per day).  1 = No; 2 = Yes
# + value range:  `range(quest1Data2$Heavy_Alcohol_Calc)`
# - Internet_Past_30 - Have you used the internet in the past 30 days?
# + value range:  1 = Yes; 2 = No


# - AlcoholDays_Past_30 - count the number of records greater than 0
# - Alcohol_Past_30 - Count Yes
# - Computed_Drinks_Day - count number of records greater than 0
# - Heavy_Alcohol_Calc - count records Yes


# col_name <- c("Drink Days Month", "Drinks Month", "Drinks in Day", "Heavy Drinking")
# row_name <- c("Internet-Yes", "Internet_no")
# counts_df <- c(AlcoholDays_Past_30_yes=AlcoholDays_Past_30_yes, Alcohol_Past_30_yes=Alcohol_Past_30_yes, Computed_Drinks_Day_yes=Computed_Drinks_Day_yes, Heavy_Alcohol_Calc_yes=Heavy_Alcohol_Calc_yes)

# counts <- c(AlcoholDays_Past_30_yes, Alcohol_Past_30_yes, Computed_Drinks_Day_yes, Heavy_Alcohol_Calc_yes)
# counts_df <-as.data.frame(counts)

# tmpdata1 <- quest1Data %>%  
#   na.omit() %>%
#   group_by(internet) %>% 
#   filter(X_rfdrhv4 == "Yes") %>% 
#   summarize(is_State=n())

tmpdata1 <- quest1Data %>%  
  na.omit() %>%
  group_by(internet) %>% 
  filter(Heavy_Drinker == "Yes", is_State == 1)
#Puerto Rico
tmpdata12 <- quest1Data %>%  
  na.omit() %>%
  group_by(internet) %>% 
  filter(Heavy_Drinker == "Yes", is_State ==2)
#Guam
tmpdata13 <- quest1Data %>%  
  na.omit() %>%
  group_by(internet) %>% 
  filter(Heavy_Drinker == "Yes", is_State ==3)
 
par(mfrow=c(1,3)) #http://www.statmethods.net/advgraphs/layout.html  dev.off()
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#ggplot(data=tmpdata1, aes(x=internet, y=X_rfdrhv4, fill=sex)) + geom_bar(stat="identity")
ggplot(data=tmpdata1, aes(x=internet, fill=sex)) + geom_bar()
ggplot(data=tmpdata1, aes(x=sex, fill=Heavy_Drinker)) + geom_bar()
ggplot(data=tmpdata1, aes(x=Heavy_Drinker, fill=sex)) + geom_bar()

#State Explore
#Puerto Rico
ggplot(data=tmpdata13, aes(x=internet, fill=sex)) + geom_bar()
ggplot(data=tmpdata13, aes(x=sex, fill=Heavy_Drinker)) + geom_bar()
ggplot(data=tmpdata13, aes(x=Heavy_Drinker, fill=sex)) + geom_bar()

#Guam
ggplot(data=tmpdata12, aes(x=internet, fill=sex)) + geom_bar()
ggplot(data=tmpdata12, aes(x=sex, fill=Heavy_Drinker)) + geom_bar()
ggplot(data=tmpdata12, aes(x=Heavy_Drinker, fill=sex)) + geom_bar()


###Research Quesion 2
#Do people that eat lots of fruits and vegetables more active than those that do not eat these healthy foods?

###Research Quesion 3
#Are people less prone to depression or anxiety if they exercise?

