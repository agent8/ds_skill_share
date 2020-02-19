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
      menuItem("Overview Map", tabName = "map", icon = icon("map")),
      menuItem("Barplots", tabName = "bar", icon = icon("bar-chart")),
      menuItem("Time Series", tabName = "ts", icon = icon("chart-line")),
      menuItem("Interactivity", tabName = "inter", icon = icon("wifi"))
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
      ),
      
      
      # Barplot
      tabItem(tabName = "bar",
              
              
      ),
      
      
      
      # Time Series
      tabItem(tabName = "ts",
              
              
      ),
      
      
      # Interactive
      tabItem(tabName = "inter",
              
              
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
  
  output$bar <- renderPlot({
    
  })
  
  output$ts <- renderPlot({
    
  })
  
  
  search_data <- reactive({
    
    search_term <- as.character(input$search)
    
    if (search_term == "") search_term <- "easter egg"
    
    foo <- gtrends(search_term, geo = "US", time = "today 1-m")
    
    df <- foo$interest_by_region %>%
      rename(state = location) %>%
      replace_na(list(hits = 0))
    
    ts <- foo$interest_over_time %>% 
        select(date, hits, keyword) %>%
        replace_na(list(hits = 0))
    
    list(search_term, df, ts)
  
}) %>% debounce(1000)
  
  
  
}
#----------------------------------------------


# run the whole enchilada
shinyApp(ui, server)
