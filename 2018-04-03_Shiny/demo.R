library(shiny)

server <- function(input,output,session){
  
  question <- reactive({
    input$questiontext
  })
  
  answers <- c("Yes","No","Maybe","Doesn't matter","Just google it")
  
  answer.out <- reactive({
    sample(answers,replace=T)
  })
  
  output$print.answer <- renderText({
    validate(
      need(
        input$askquestion > 0, "Please ask a question"
      )
    )
    x <- sample(1:5,1)
    answers[x]
  })
  
  observeEvent(input$askquestion,{
    if(input$askquestion>0){
      #answer.out <- sample(answers,replace=T)
      "cool"
    } 
  })
}

ui <- fluidPage(
  titlePanel("Question Answerer"),
  tagList(
    fluidRow(
      h2("Ask me anything!"),
      textInput(inputId = "questiontext",label = "Type your question here",value = ""),
      actionButton("askquestion",label = "ASK!"),
      textOutput("print.answer")
    )
  )
)

