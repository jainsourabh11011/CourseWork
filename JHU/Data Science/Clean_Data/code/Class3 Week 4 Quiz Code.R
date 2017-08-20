# Question 1
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() 
# from here: 
#      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# 
# and load the data into R. The code book, describing the variable names is here: 
#      
#      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# 
# Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
# What is the value of the 123 element of the resulting list?

if(!file.exists("./data")){dir.create("./data")}
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "./data/idahohousing.csv")
download.file(url1,f)
idahohousing <- read.csv("./data/idahohousing.csv")

# Get the names fro each column and strip wgtp from the name if the strig exists
splitnames=strsplit(names(idahohousing),"wgtp")
# Get the name of the 123rd column
splitnames[[123]]

#-------------------------------
# Question 2:
# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#      
#      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# 
# Remove the commas from the GDP numbers in millions of dollars and average them. What is the average? 
# 
# Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table 

if(!file.exists("./data")){dir.create("./data")}
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f <- file.path(getwd(), "./data/gdp.csv")
download.file(url1,f)
gdp <- read.csv("./data/gdp.csv")

# Column X.3 holds the values in millions with commas - 5th col
# The first 4 rows do not include important data
# after row 194, there is other info not needed
# gsub is like sub.  sub finds the first instance of the old substring within text and replaces
# it with new.  gsub does the same thing but replaces all instances of the substring
cleanedData <- gsub(",", "", gdp[5:194, 5])
# Since cleanedData is class character, need to chage it to numeric
cleanedData <- as.numeric(cleanedData)
mean(cleanedData)

#-------------------------------
# Question 3:

# In the data set from Question 2 what is a regular expression that would allow you to count 
# the number of countries whose name begins with "United"? Assume that the variable with the 
# country names in it is named countryNames. How many countries begin with United?

# Country names are in the 4th column
countryNames <- gdp[5:194,4]
# Easiest
length(grep("United", countryNames))
# Also easy
table(grepl("United", countryNames))
# with regular expression - use ^United - provide sthe first word United
length(grep("^United", countryNames))

#-------------------------------
# Question 4:

# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#      
#      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# 
# Load the educational data from this data set: 
#      
#      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
# 
# Match the data based on the country shortcode. Of the countries for which the end of the fiscal 
# year is available, how many end in June? 
 
if(!file.exists("./data")){dir.create("./data")}
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f <- file.path(getwd(), "./data/gdp.csv")
download.file(url1,f)
gdp <- read.csv("./data/gdp.csv", skip=4. rows=190) # Remeber there is some data of no use in csv

if(!file.exists("./data")){dir.create("./data")}
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f <- file.path(getwd(), "./data/edu.csv")
download.file(url2,f)
edu <- read.csv("./data/edu.csv")

# merge the datasets
merged <- merge(gdp, edu, by.x = 'X', by.y = 'CountryCode')

# extract the information
fy.june <- grep('Fiscal year end: June', merged$Special.Notes)
length(fy.june)

#-------------------------------
# Question 5:

# You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices for 
# publicly traded companies on the NASDAQ and NYSE. Use the following code to download data on 
# Amazon's stock price and get the times the data was sampled. 
# 
# library(quantmod)
# amzn = getSymbols("AMZN",auto.assign=FALSE)
# sampleTimes = index(amzn) 
# 
# How many values were collected in 2012? How many values were collected on Mondays in 2012?

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

# create logical for year 2012
year2012 <- grepl('2012-*', sampleTimes)

# count 2012 observations boolean (i.e. true - false)
table(year2012)

# subset based on 2012
sampleTimes2012 <- subset(sampleTimes, year2012)
# Or
sampleTimes2012.2 <- sampleTimes[year2012]

# convert to day of week
day <- format(sampleTimes2012.2, '%A')

# count each day
table(day)
# Or
ct <- day[day=="Monday"]






