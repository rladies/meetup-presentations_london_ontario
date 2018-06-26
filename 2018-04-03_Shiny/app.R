library(shiny)

ui <-  fluidPage(
  titlePanel("Magic 8 Ball"),
  hr(),
  mainPanel(
    tagList(
      fluidRow(
        h4("Ask me anything!"),
        textInput(inputId = "questiontext",label = "Type your question here:",value = ""),
        actionButton("askquestion",label = "ASK!"),
        br(),
        br(),
        textOutput("print.answer"),
        br(),
        br(),
        br(),
        br(),
        br(),
        hr(),
        actionButton("snapshot",label = "Debugger")
      )
    )
  )
)

server <- function(input,output){
  
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

shinyApp(ui = ui, server = server)



