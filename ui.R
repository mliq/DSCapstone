library(shiny)
options(shiny.trace = F)  # cahnge to T for trace
library(shinysky)

shinyUI(fluidPage( #"Predictor",
  
  # Supress Error messages
  tags$style(type="text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
  ),
  # Application title
  h1("Predictor!"),
  #h5("Please allow 10-20 seconds for the initial loading of the prediction engine"),
  hr(),
  busyIndicator("Prediction In progress",wait = 0),
   # INPUT
  HTML('<textarea id="text" rows="3" cols="440"></textarea>'),
  #tags$textarea(id="text", rows=2, cols=260, ""),
  #textInput("text",label=h4("Enter Your Text Below:")),
  textOutput("prediction"),
  checkboxInput("checkbox", label = "Enhance Accuracy (Classify Text, add ~ 3-5 secs)", value = FALSE)
  
)                          
)
