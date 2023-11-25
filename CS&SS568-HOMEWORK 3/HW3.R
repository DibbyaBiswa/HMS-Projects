# CS & SS 569 HOMEWORK 3
# DIBBYA BISWA
# 2023-02-27

library(shiny)
library(ggplot2)
library(dplyr)

# load data
data <- read.csv("data_2003_merged.csv", header = TRUE)

# ui
ui <- fluidPage(
  titlePanel("Average Number of Bad Teeth in 2003 Among 12 years old"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "continent",
        label = "Continent(s) to include and display",
        choices = unique(data$continent),
        selected = "Asia" # default
      ),
      selectInput(
        inputId = "yvar",
        label = "Outcome you want to show",
        choices = c(
         # "Life Expectancy" = "life_expectancy",
         # "Bad Teeth Average" = "bad_teeth_avg",
          "Country" = "country"
        ),
        selected = "Life Expectancy"
      )
    ),
    mainPanel(
      plotOutput("GMplot")
    )
  )
)

# server
server <- function(input, output) {
  output$GMplot <- renderPlot({
    data_filtered <- data %>%
      filter(continent %in% input$continent)
    
    ggplot(data_filtered, aes(x = bad_teeth_avg, y = life_expectancy, color = continent, label = country)) +
      geom_point() +
      geom_text(check_overlap = T) +
      scale_color_discrete(name = "Continent") +
      labs(x = "Average Number of Bad Teeth in 2003", y = "Life Expectancy of 2003")
  })
}

# Run app
shinyApp(ui = ui, server = server)

