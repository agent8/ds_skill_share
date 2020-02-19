# Don't change this block
# ================
# required libraries
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(viridis)
library(usmap)
library(gtrendsR)

# Load data:
df <- read_csv("./data/gtrends.csv")
ts <- read_csv("./data/gtrends_ts.csv")

# some settings
cmaps <- c("viridis", "magma", "inferno", "plasma", "cividis")
cmap_range <- range(df$hits)
date_range <- range(ts$date)
# ================


# Shiny Code
#==============================================

# UI
#----------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Fun with maps"),
  
  # sidebar tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview Map", tabName = "map", icon = icon("map"))
    )
  ),
  
  # all the UI contents in the individual tabItems
  dashboardBody(
    tabItems(

      # Map view
      tabItem(tabName = "map",
              
              fluidRow(
                column(width = 9,
                       box(plotOutput(outputId = "map", height = 550), width = "80%")
                ),
                
                column(width = 3,
                       box(width = "20%",
                           selectInput(inputId = "cmap", label = "Colour Map", 
                                       choices = cmaps, 
                                       selected = "viridis"),
                           sliderInput(inputId = "cmap_range", label = h3("Colour Range"),
                                       min = 0, max = 1, 
                                       value = c(0,1))

                       ) 
                )
              )
      )
      
    )
  )
)
#----------------------------------------------


# Server Side
#----------------------------------------------

server <- function(input, output) {
  
  output$map <- renderPlot({
    plot_usmap(data = df, values = "hits",  color = "grey20", labels=TRUE) + 
      scale_fill_viridis(option = input$cmap, begin = input$cmap_range[1], end = input$cmap_range[2]) +
      theme(legend.position = "right", title = element_text(size = 20),legend.text = element_text(size = 11)) + 
      labs(fill = "Popularity", title = "Popularity of Google Search 'superbowl' by State")
  })
  
}
#----------------------------------------------


# run the whole enchilada
shinyApp(ui, server)
