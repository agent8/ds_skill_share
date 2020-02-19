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
___ <- fluidPage(
  ___("Regression Demo"),
  sidebarLayout(
    ___Panel(
      
      ___Input(inputId = "feat", label = "Feature", 
                  choices = pred_list, 
                  selected = "cyl"),
      
      radioButtons(___, label = "Colour",
                   choices = ___, 
                   selected = col_list[1], inline = FALSE)
      
    ),
    mainPanel(plotOutput(___ = "scatter"))
  )
)

# server-side computations
server <- function(___, ___) {
  output$___ <- renderPlot({
    
    # selecting the 2 variables to plot
    foo <- df %>% 
      select(mpg, x = input$___)
    
    # plotting in our chosen colour
    foo %>% 
      ggplot(aes(mpg, x)) +
      geom_smooth(method = "lm", col = "black") +
      geom_point(size = 3, col = ___) +
      ggtitle(glue("Plotting mpg vs feature '{ input$feat }'"))

  })
}

# run with: shiny::runApp("02_shiny_reg")
___(ui = ui, ___)

