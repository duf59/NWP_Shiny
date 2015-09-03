## ui.R ##
fluidPage(
    titlePanel("Next Word Prediction App"),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "text", label = h3("Text input"), value = "...")
        ),
        mainPanel(
            h1("Prediction:"),
            tableOutput("prediction")
        )
    )
)
