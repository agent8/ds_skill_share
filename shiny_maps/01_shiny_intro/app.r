# required libraries
library(shiny)

# user interface elements and layout
ui <- fluidPage(
  titlePanel("Heads or Tails"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "n", label = "Number of flips:",
                  min = 1, max = 1000, value = 500),
      sliderInput(inputId = "prob", label = "Success probability:",
                  min = 0, max = 1, value = 0.5),
      
    ),
    mainPanel(plotOutput(outputId = "bars"))
  )
)

# server-side computations
server <- function(input, output) {
  output$bars <- renderPlot({
    
    flips <- table(rbinom(input$n, 1, input$prob))
    
    barplot(flips, main = "Coin flip results")
  })
}

# run with: shiny::runApp("01_shiny_intro")
shinyApp(ui = ui, server = server)