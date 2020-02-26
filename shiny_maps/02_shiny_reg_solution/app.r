# Don't change this block
# ================
# required libraries
library(shiny)
library(dplyr)
library(stringr)
library(glue)
library(ggplot2)

# data set + predictor features + colours
df <- mtcars
pred_list <- df %>% select(-mpg) %>% colnames()
col_list <- sample(colors()[!str_detect(colors(), "gr.y")], 9)
# ================

# user interface elements and layout
ui <- fluidPage(
  titlePanel("Regression Demo"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "feat", label = "Feature", 
                  choices = pred_list, 
                  selected = "cyl"),
      
      radioButtons(inputId = "col", label = "Colour",
                   choices = col_list, 
                   selected = col_list[1], inline = FALSE)
      
    ),
    mainPanel(plotOutput(outputId = "scatter"))
  )
)

# server-side computations
server <- function(input, output) {
  output$scatter <- renderPlot({
    
    foo <- df %>% 
      select(mpg, x = input$feat)
    
    foo %>% 
      ggplot(aes(mpg, x)) +
      geom_smooth(method = "lm", col = "black") +
      geom_point(size = 3, col = input$col) +
      ggtitle(glue("Plotting mpg vs feature '{ input$feat }'"))

  })
}

# run with: shiny::runApp("02_shiny_reg")
shinyApp(ui = ui, server = server)
