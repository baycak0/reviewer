library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Trend Visualizer"),
  sidebarLayout( 
    sidebarPanel(
      fileInput("file", "Upload a CSV file"),
      # Define input elements for x and y variables
      selectInput("x_variable", "X-Axis Variable:", ""),
      selectInput("y_variable", "Y-Axis Variable:", ""),
      selectInput("color_variable", "Color Variable:", ""),
      selectInput("shape_variable", "Shape Variable:", "")
    ),
    mainPanel(theme = shinytheme("spacelab"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Read the uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Populate X and Y parameter choices based on column names
  observe({
    col_names <- colnames(data())
    updateSelectInput(session = getDefaultReactiveDomain(), "x_variable", choices = col_names)
    updateSelectInput(session = getDefaultReactiveDomain(), "y_variable", choices = col_names)
    updateSelectInput(session = getDefaultReactiveDomain(), "color_variable", choices = col_names)
    updateSelectInput(session = getDefaultReactiveDomain(), "shape_variable", choices = col_names)
  })
  
  # Generate the plot
  output$plot <- renderPlot({
    df <- data()
    
    ggplot(df, aes(x = df[[input$x_variable]], y = df[[input$y_variable]], 
                        color = as.factor(df[[input$color_variable]]),
                        shape = as.factor(df[[input$shape_variable]]), group = interaction(df[[input$color_variable]], df[[input$shape_variable]]))) +
      geom_line() +
      geom_point() +
      labs(x = paste0(input$x_variable), y = paste0(input$y_variable), shape = paste0(input$shape_variable),  color = paste0(input$color_variable)) + theme_minimal()
    
  })
}

shinyApp(ui, server)
