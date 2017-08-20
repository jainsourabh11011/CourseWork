#options(shiny.trace=TRUE, shiny.autoreload=TRUE, shiny.error=recover)
library(shinythemes)
library(wordcloud)
shinyUI(navbarPage("JHU Data Science Capstone", theme = shinytheme("united"),
                   # Using Tab interface since I have a number of documents to share
                   
                   tabPanel("Guess Word",
                            sidebarLayout(
                                 sidebarPanel(
                                      includeHTML("notes/sidePanelInclude.html"),

                                      radioButtons("chooseCloud", "Select WordCloud:",
                                                   c("with Stop Words" = "1",
                                                     "No Stop Words" = "2"))
                                 ),
                            mainPanel(
                                 textInput("text", label = h3("Please enter some words below:")),
                                 tags$hr(),
                                 tags$h3("I guess the next word might be:"),
                                 #(tags$h4(textOutput("guessedWord")))
                                 (tags$h3(div(textOutput("guessedWord"), style="color:#bf3e11"))),
                                 
                                 imageOutput("wordCloud")
                                 
                            )
                   )
),
tabPanel("About", 
         includeMarkdown("./notes/myPredictionApp.html")
),

tabPanel("Text Analytics", 
         includeMarkdown("./notes/textAnalytics.html")
),

tabPanel("How it Works", 
         includeMarkdown("./notes/HowItWorks.html")
),

tabPanel("Next Steps", 
         includeMarkdown("./notes/nextSteps.html")
)
)
)