#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

data <- read.csv("songs.csv")

data_clean <- data %>%
  filter(!is.na(year),
         !is.na(artist),
         !is.na(top.genre)) %>%
  mutate(
    genre_group = case_when(
      str_detect(tolower(top.genre), "pop") ~ "Pop",
      str_detect(tolower(top.genre), "rap") ~ "Rap",
      str_detect(tolower(top.genre), "rock") ~ "Rock",
      TRUE ~ "Other"
    )
  )

ui <- fluidPage(
  
  titlePanel("Evolution of Musical Features in Top Streamed Songs"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "genre",
        "Select Genre:",
        choices = c("All","Pop","Rap","Rock","Other"),
        selected = "All"
      )
    ),
    
    mainPanel(
      p("This visualization explores how musical characteristics of popular songs have changed over time across genres."),
      p("Among the features, song length appears to show a slight decreasing trend over time, while most other features show weaker patterns."),
      plotOutput("feature_plot")
    )
    
  )
)

server <- function(input, output) {
  
  genre_filtered <- reactive({
    
    df <- data_clean
    
    if (input$genre != "All") {
      df <- df %>%
        filter(genre_group == input$genre)
    }
    
    df
    
  })
  
  output$feature_plot <- renderPlot({
    
    req(nrow(genre_filtered()) > 0)
    
    genre_filtered() %>%
      pivot_longer(
        cols = c(
          energy,
          danceability,
          valance,
          beats.per.minute,
          length,
          acousticness
        ),
        names_to = "metric",
        values_to = "value"
      ) %>%
      ggplot(aes(year, value)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      
      facet_wrap(~metric, scales = "free_y") +
      labs(
        title = paste("Audio Feature Trends for Genre:", input$genre),
        subtitle = "Based on Spotify Top Streamed Songs",
        x = "Year",
        y = "Value"
      ) +
      theme_minimal()
    
  })
  
}

shinyApp(ui, server)
