#setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Predictor")

#library(shiny)
#options(shiny.trace = F)  # cahnge to T for trace
#library(shinysky)

load("no4CountsCompiledFuncs.RData")  

shinyServer(function(input, output) {
  
  
  
  # OUTPUT PREDICTION #
  
  output$prediction <- renderText({  
    as.character(getPred.(input$text))
  })
})