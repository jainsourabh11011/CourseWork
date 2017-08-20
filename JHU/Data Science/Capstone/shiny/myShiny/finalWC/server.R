source("./myPrediction.R")

shinyServer(function(input, output) 
{
     myGuess <- reactive({
          # Get the input from the UI
          text <- input$text
          textInput <- cleanText(text)
          wordCount <- length(textInput)
          myGuess <- myPrediction(wordCount,textInput)
          })
        output$guessedWord <- renderText(myGuess()) #works

        output$wordCloud <- renderImage({
             filename <- normalizePath(file.path("./www",
                                       paste("cloud", input$chooseCloud, ".png", sep = "")))
             list(src = filename,
                  alt = paste("Image number", input$n))
             
        }, deleteFile = FALSE)

})