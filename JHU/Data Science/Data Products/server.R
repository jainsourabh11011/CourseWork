library(shiny)
library("dplyr")
library("ggplot2")
library("scales")
library("readr")

rawData = read_csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/marijuana-street-price-clean.csv")

data <- tbl_df((rawData))
data <- select(data, -c(HighQN, MedQN, LowQN))

data$Month <- as.Date(cut(data$date, breaks = "month"))
data$Week <- as.Date(cut(data$date, breaks = "week", start.on.monday = TRUE)) # changes weekly break point to Sunday

data_MD <- filter(data, State=="Maryland")

shinyServer(
  function(input, output) {
  
  output$mainPlot <- renderPlot({
    dataState=subset(data, State==input$varState)
    monthplot <- ggplot(data=dataState, aes(Week, HighQ)) +
      stat_summary(fun.y = mean, geom = "line") +
      scale_x_date(breaks = date_breaks("8 week"), labels = date_format("%m-%Y")) +
      labs(y = "Marijuana Ave Price $") + geom_smooth(colour = "red")
    monthplot
  })

  output$varState <- renderText({ 
    paste("Showing data for: ", input$varState)
    })
    
}
)


#References
#RStudio Shiny Tutorial
##http://shiny.rstudio.com/gallery/widget-gallery.html