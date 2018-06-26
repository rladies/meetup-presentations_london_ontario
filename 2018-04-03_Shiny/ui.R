shinyUI(
  fluidPage(
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
)
