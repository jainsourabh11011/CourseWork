url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f <- file.path(getwd(), "GDP.csv")
download.file(url1, f)
gdp <- read.csv("gdp.csv")

# Clean the data - has a few row on the top and trash at the botton
gdpclean<-gdp[5:194,]
# The Gross.domestic.product.2012 column is not numeric.  Cannot sort if not changed
# Rseek search suggests this is the best solution:  as.numeric(as.character(x))
gdpclean$Gross.domestic.product.2012 = as.numeric(as.character(gdpclean$Gross.domestic.product.2012))

url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl1, destfile = "edu.csv", method = "auto")
edu <- read.csv("edu.csv")

intersect(names(gdp), names(edu)) # does not work because no shared column names
#gdp[ setdiff( names(gdp), names(edu)) ]

names(gdp)
names(edu)

# X in gdp is the country shortcode
# mergedData=as.data.frame(merge(gdpclean,edu,by.x="X",by.y="CountryCode"))
mergedData=(merge(gdpclean,edu,by.x="X",by.y="CountryCode"))

# arrange depends on dplyr
sortedData <- arrange(mergedData, desc(Gross.domestic.product.2012))

# Get 13 row and 4 th column to get country
print(sortedData[13,4])


