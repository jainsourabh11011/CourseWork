library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Census Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Census Regions", "Census Divisions")),
      
      radioButtons("censusyear", "Census Year", c(2000, 2010)),
      br(),
      br(),
      includeHTML("interesting.html")
      ),

    mainPanel(

      tabsetPanel(
      
      h4("Selected Census Data"),
      tabPanel("Data Table", DT::dataTableOutput("view"), value="thistab"),
      tabPanel("Census Plot", plotOutput("dataplot")),
      tabPanel("How To", includeMarkdown("datanotes_reg.Rmd")),
      selected = "thistab"
      
      )
      
    )
  )
))