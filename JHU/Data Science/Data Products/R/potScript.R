
library("dplyr")
library("ggplot2")
library("scales")
library("readr")

rawData = read_csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/marijuana-street-price-clean.csv")

data <- tbl_df((rawData))
data <- select(data, -c(HighQN, MedQN, LowQN))


#data$date <- as.Date(format(data$date, format = "%B%Y")) #change to character from date class

#data$date = as.Date(as.character(data$date), format="%B%Y")
data$Month <- as.Date(cut(data$date, breaks = "month"))
data$Week <- as.Date(cut(data$date, breaks = "week", start.on.monday = TRUE)) # changes weekly break point to Sunday

data_MD <- filter(data, State=="Maryland")

# graph by month:

monthplot <- ggplot(data = data, aes(Week, HighQ)) +
  stat_summary(fun.y = mean, geom = "line") +
  scale_x_date(breaks = date_breaks("8 week"), labels = date_format("%m-%Y")) +
  labs(y = "Marijuana Ave Price")
monthplot = monthplot + geom_smooth(colour = "red")

monthplot

p = ggplot(data = data_MD, aes(x = MonthYear, y = meanprice)) + geom_line()
#p = p + geom_point(alpha = .05) # add points
p = p + geom_line() # add points
p = p + geom_smooth(colour = "red")
#p = p + facet_wrap(~ State, scales = "free_y")
#p = p + scale_x_date(breaks = pretty_breaks(4))
p = p + labs(x = NULL, y = "Price ($)", title = "Price of Marijuana")

log <- data.frame(Date = c("2013/05/25","2013/05/28","2013/05/31","2013/06/01","2013/06/02","2013/06/05","2013/06/07"), 
                    Quantity = c(9,1,15,4,5,17,18))
log
str(log)

log$Date <- as.Date(log$Date, "%Y/%m/%d")

# create variables of the week and month of each observation:
log$Month <- as.Date(cut(log$Date, breaks = "month"))
log$Week <- as.Date(cut(log$Date, breaks = "week", start.on.monday = FALSE)) # changes weekly break point to Sunday

str(log)

# graph by month:
ggplot(data = log,
       aes(Month, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") # custom x-axis labels

rawData %>%
  group_by(State) %>%
  summarise(
    m.price = mean(HighQ, na.rm = TRUE)
  ) %>%
  arrange(desc(m.price))


rawData %>%
  group_by(State) %>%
  summarise(
    m.price = mean(HighQ, na.rm = TRUE)
  ) %>%
  arrange(desc(m.price))

p = ggplot(rawData, aes(x = date, y = HighQ))
p = p + geom_point(alpha = .05) # add points
p = p + geom_line() # add points
p = p + geom_smooth(colour = "red")
p = p + facet_wrap(~ State, scales = "free_y")
p = p + scale_x_date(breaks = pretty_breaks(4))
p = p + labs(x = NULL, y = "Price ($)", title = "Price of Marijuana")
