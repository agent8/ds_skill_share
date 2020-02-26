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
              
              fluidRow(
                column(width = 9,
                       box(plotOutput(outputId = "bar", height = 550), width = "80%")
                ),
                
                column(width = 3,
                       box(width = "20%",
                           radioButtons(inputId = "cmap_bar", label = "Colour Map",
                                        choices = cmaps,
                                        selected = "viridis", inline = TRUE),
                           sliderInput(inputId = "cmap_range_bar", label = h3("Colour Range"),
                                       min = 0, max = 1, 
                                       value = c(0,1))

                       ) 
                )
              )
      ),
      
      
      
      # Time Series
      tabItem(tabName = "ts",
              
              fluidRow(
                column(width = 9,
                       box(plotOutput(outputId = "ts", height = 550), width = "80%")
                ),
                
                column(width = 3,
                       box(width = "20%",
                           dateRangeInput(inputId = "ts_dates", label = h3("Date range"),
                                          start = date_range[1], end = date_range[2],
                                          min = date_range[1], max = date_range[2]
                                          )

                       ) 
                )
              )
      ),
      
      
      # Interactive
      tabItem(tabName = "inter",
              
              fluidRow(
                column(width = 10,
                       box(plotOutput(outputId = "inter_map", height = 350), width = "80%")
                ),
                
                column(width = 2,
                       box(width = "20%",
                           textInput(inputId = "search", label = h3("Search term"), value = "superbowl")

                       ) 
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(plotOutput(outputId = "inter_ts", height = 200), width = "100%")
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
  
  output$bar <- renderPlot({
    df %>% 
      ggplot(aes(state, hits, fill = hits)) +
      geom_col() +
      scale_fill_viridis(option = input$cmap_bar, begin = input$cmap_range_bar[1], end = input$cmap_range_bar[2]) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.9),
            title = element_text(size = 20), axis.text = element_text(size = 11)) + 
      labs(fill = "Popularity", title = "Popularity of Google Search 'superbowl' by State", x = "", y = "")
    
  })
  
  output$ts <- renderPlot({
    ts %>% 
      filter(between(lubridate::date(date), lubridate::date(input$ts_dates[1]), lubridate::date(input$ts_dates[2]))) %>% 
      ggplot(aes(date, hits)) +
      geom_line(col = "blue", size = 2) +
      labs(title = "Popularity of Google search 'superbowl' over time", x = "", y = "") +
      theme(title = element_text(size = 15))
    
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
  
  
  output$inter_map <- renderPlot({
    
    plot_usmap(data = search_data()[[2]], values = "hits",  color = "grey20", labels=TRUE) + 
      scale_fill_viridis(option = "viridis") +
      theme(legend.position = "right", title = element_text(size = 20),legend.text = element_text(size = 11)) + 
      labs(fill = "Popularity", title = str_c("Popularity of Google Search '",
                                              search_data()[[1]],
                                              "' by State"))
  })
  
  output$inter_ts <- renderPlot({
    search_data()[[3]] %>% 
      ggplot(aes(date, as.numeric(hits))) +
      geom_line(col = "blue", size = 2) +
      labs(title = str_c("Popularity of Google Search '",
                         search_data()[[1]],
                         "' over time"), x = "", y = "") +
      theme(title = element_text(size = 15))
    
  })
  
}
#----------------------------------------------


# run the whole enchilada
shinyApp(ui, server)
