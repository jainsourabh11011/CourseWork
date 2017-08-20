# Word Predict - Shiny Server module
#
# Author: filqua74

library(shiny)

source("modules/main.R")

shinyServer(
  function(input, output, session) {
    reac <- reactiveValues(phrase = isolate(input$phrase))
    
    observe({
      tmp <- input$phrase
      if (substr(tmp,nchar(tmp),nchar(tmp))==" ") {
        reac$phrase <- tmp
      }
    })
    
    
    output$predWords <- renderUI({
                          HTML(paste(wordPredict(reac$phrase,ncand=10)$word,collapse = "<br/>"))
                        })
  }
)
