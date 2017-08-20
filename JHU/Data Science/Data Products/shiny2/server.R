library(shiny)
library(datasets)
library(dplyr)

options(encoding = "UTF-8") #required to avoid: warning in readLines(con) incomplete final line

#need to consider http://shiny.rstudio.com/articles/datatables.html
options("scipen"=100)
cen_reg_2000 <- read.csv("../shiny2/CensusRegions2000.csv", header = TRUE)
cen_reg_2010 <- read.csv("../shiny2/CensusRegions2010.csv", header = TRUE)
cen_div_2000 <- read.csv("../shiny2/CensusDivisions2000.csv", header = TRUE)
cen_div_2010 <- read.csv("../shiny2/CensusDivisions2010.csv", header = TRUE)

cen_reg_2000 <- select(cen_reg_2000, -c(MTFCC:REGION), -LSADC, -FUNCSTAT, -HU100, -c(CENTLAT:INTPTLON))
cen_reg_2010 <- select(cen_reg_2010, -c(MTFCC:REGION), -LSADC, -FUNCSTAT, -HU100, -c(CENTLAT:INTPTLON))
cen_div_2000 <- select(cen_div_2000, -c(MTFCC:DIVISION), -LSADC, -FUNCSTAT, -HU100, -c(CENTLAT:INTPTLON))
cen_div_2010 <- select(cen_div_2010, -c(MTFCC:DIVISION), -LSADC, -FUNCSTAT, -HU100, -c(CENTLAT:INTPTLON))

basename_reg <-c("Northeast", "Midwest", "South", "West")
pop_reg <- c(3.21, 3.94, 14.29, 13.84)

basename_div <- c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", 
                  "East South Central", "West South Central", "Mountain", "Pacific")
pop_div <-c(3.75, 3.03, 2.80, 6.59, 15.47, 8.28, 15.59, 21.42, 10.78)

# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  
  datasetInput <- reactive({
    if(input$censusyear==2000){
      switch(input$dataset,
             "Census Regions" = cen_reg_2000,
             "Census Divisions" = cen_div_2000)        
    } else{
      switch(input$dataset,
             "Census Regions" = cen_reg_2010,
             "Census Divisions" = cen_div_2010) 
    }
    
  })
  
popdataInput<-reactive({
  if(input$dataset=="Census Regions"){
    popdata=pop_reg
    
  } else{
    popdata=pop_div
  }
})


basenameInput<-reactive({
  if(input$dataset=="Census Regions"){
    basename=basename_reg
    
  } else{
    basename=basename_div
  }
})

  output$view <- DT::renderDataTable({
    DT::datatable(datasetInput(), options=list(searching=FALSE, paging=FALSE, rownames=FALSE))
  })

    
  output$dataplot <- renderPlot(height=500,{
    par(mar=c(8.3, 4.1, 2, 1))
    barplot(popdataInput(), names.arg = basenameInput(),  
                    main = "Population Change Between 2000 & 2010", ylab="Population Growth %", 
                    col="#7FFFD4", border = "#7FFFD4", las=2) 
})
})
