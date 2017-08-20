# Word Predict - Shiny UI module
#
# Author: filqua74

library(shiny)

inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value))
  )
}

shinyUI(fluidPage(
  fluidRow(
    column(width=10,
           h2("Word Prediction App - Capstone Project")
    )
  ),
  fluidRow(
    column(width=10,
           h4("Coursera Data Science Specialization")
    )
  ),
  fluidRow(
    column(width=10,
           h4("Author: filqua74 - April 2016")
   )
  ),
  fluidRow(
    column(width=4,
    h3("Entering text:"),
    inputTextarea('phrase','',20,40)
    ),
    column(width=8,
    h3("Suggested words ( ordered by likelihood)"),
    htmlOutput('predWords')
    )
  )
))
