#demo shiny app 

library(shiny)

server <- function(input,output,session){
  
  question <- reactive({
    input$questiontext
  })
  
  answers <- c("Yes","No","Maybe","Doesn't matter","Just google it")
  
  output$print.answer <- renderText({
    validate(
      need(
        input$askquestion > 0, "Please ask a question"
      )
    )
    x <- sample(1:5,1)
    answers[x]
  })

  observeEvent(input$snapshot, {
    browser()
  })
}






