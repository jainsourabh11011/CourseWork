#ui.R
shinyUI(fluidPage(
  titlePanel("Marijuana Street Prices by State"),
  sidebarLayout(
    sidebarPanel(
      h2("State Selection"),
      selectInput("varState", 
            label = "Choose a State",
            choices = list("North Carolina", "Maryland",
                                 "Arizona", "Pennsylvannia"),
            selected = "North Carolina"),
      
     dateRangeInput("dateRange", label = "Date Range", min = "2013-12-01", max = "2015-05-01", 
          format="M-yy", start = "2013-12-01", end = "2015-07-01", separator= "-"),
      
      br(),
      br(),
      br(),
      br(),
      img(src = "pot.jpg", height = 72, width = 72)
    ),      
  mainPanel(
    h3("Graph by State"),
    plotOutput("mainPlot"),
    textOutput("varState")
    
))))