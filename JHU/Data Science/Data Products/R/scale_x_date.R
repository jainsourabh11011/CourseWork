library(ggplot2)
library(scales)

# load data:
log <- data.frame(Date = c("2013/05/25","2013/05/28","2013/05/31","2013/06/01","2013/06/02","2013/06/05","2013/06/07"), 
                  Quantity = c(9,1,15,4,5,17,18))
log
str(log)

# convert date variable from factor to date format:
log$Date <- as.Date(log$Date,
                    "%Y/%m/%d") # tabulate all the options here
str(log)

# create variables of the week and month of each observation:
log$Month <- as.Date(cut(log$Date,
                         breaks = "month"))
log$Week <- as.Date(cut(log$Date,
                        breaks = "week",
                        start.on.monday = FALSE)) # changes weekly break point to Sunday
log

# graph by month:
ggplot(data = log,
       aes(Month, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") # custom x-axis labels

# graph by week:
ggplot(data = log,
       aes(Week, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "1 week") # custom x-axis labels


#http://stackoverflow.com/questions/32653730/ggplot2-scale-x-date?noredirect=1#comment53155072_32653730

# graph by month:
ggplot(data = log,
       aes(Month, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(breaks = date_breaks("1 month"))


#See https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

last_month <- Sys.Date() - 0:29
df <- data.frame(
  date = last_month,
  price = runif(30)
)
base <- ggplot(df, aes(date, price)) +
  geom_line()
base
# The date scale will attempt to pick sensible defaults for
# major and minor tick marks. Override with date_breaks, date_labels
# date_minor_breaks arguments.
base + scale_x_date(date_labels = "%b %d")
base
base + scale_x_date(date_breaks = "1 week", date_labels = "%W")
base
base + scale_x_date(date_minor_breaks = "1 day")
base
# Set limits
base + scale_x_date(limits = c(Sys.Date() - 7, NA))
base

