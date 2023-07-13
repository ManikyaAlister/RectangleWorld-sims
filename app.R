library(shiny)
library(shinythemes)
library(here)
library(tidyverse)

# Source the plotShinyHeatMaps function
source(here("shinyPlottingFunctions.R"))

# Function to load data_cartesian based on the experiment
loadDataCartesian <- function(experiment) {
  file_path <- paste0("experiment-", experiment, "/data/derived/data_cartesian.Rdata")
  load(file_path)
  return(d_cartesian)
}

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title
                titlePanel("Heatmaps of Learner Rectangles"),
                
                # CSS styles
                tags$head(
                  tags$style(
                    HTML("
        .plot-outline {
          border: 1px solid #999;
          padding: 5px;
          margin-bottom: 10px;
          
          
        }
      ")
                  )
                ),
                  
                  # Main panel
                  mainPanel(
                    # selection panel
                    fluidRow(
                      # Experiment selection
                      column(3, selectInput("experiment", "Experiment", choices = c(1, 2), selected = 1)),
                      # Block selection
                      column(3, selectInput("b", "Block", choices = c(1, 2, 3, 4, 5, 6, 7, 8), selected = 1)),
                      # Clue number selection
                      column(3, selectInput("clueNum", "Clue Number", choices = c(1, 2, 3, 4), selected = 1))
                      

                    ),
                    # Heatmap plots and labels
                    fluidRow(
                      h3("Model Predictions")
                    ),
                    fluidRow(
                      column(3, div(plotOutput("plotSim1", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plotSim2", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plotSim3", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plotSim4", height = "250px"), class = "plot-outline"))
                    ),
                    fluidRow(
                      h3("Experiment Data")
                    ),
                    fluidRow(
                      column(3, div(plotOutput("plot1", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot2", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot3", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot4", height = "250px"), class = "plot-outline"))
                    ),
                    fluidRow(
                      column(3, div(plotOutput("plot5", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot6", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot7", height = "250px"), class = "plot-outline")),
                      column(3, div(plotOutput("plot8", height = "250px"), class = "plot-outline"))
                    )
                    
                  )
                
)



# Define server
server <- function(input, output) {
  
  # Reactive expression to load data_cartesian
  d_cartesian <- reactive({
    experiment <- input$experiment
    loadDataCartesian(experiment)
  })
  
  
  # Simulated data 
  output$plotSim1 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "HS", clueNum = input$clueNum, experiment = "sim")
  })
  
  output$plotSim2 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "RS", clueNum = input$clueNum, experiment = "sim")
  })
  
  output$plotSim3 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "MS", clueNum = input$clueNum, experiment = "sim")
  })
  
  output$plotSim4 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "US", clueNum = input$clueNum, experiment = "sim")
  })
  
  # Experiment data
  
  # Plot 1
  output$plot1 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "HS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 2
  output$plot2 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "RS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 3
  output$plot3 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "MS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 4
  output$plot4 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "US", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 5
  output$plot5 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "HN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 6
  output$plot6 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "RN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 7
  output$plot7 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "MN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 8
  output$plot8 <- renderPlot({
    plotShinyHeatMaps( b = input$b, condition = "UN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Reactive expression to generate labels
  labels <- reactive({
    conditions <- c("HS", "RS", "MS", "US", "HN", "RN", "MN", "UN")
    selected_conditions <- c("HS", "RS", "MS", "US", "HN", "RN", "MN", "UN")
    labels <- ifelse(selected_conditions %in% conditions, selected_conditions, "")
    return(labels)
  })
  
  # Output labels
  output$label1 <- renderText({ labels()[1] })
  output$label2 <- renderText({ labels()[2] })
  output$label3 <- renderText({ labels()[3] })
  output$label4 <- renderText({ labels()[4] })
  output$label5 <- renderText({ labels()[5] })
  output$label6 <- renderText({ labels()[6] })
  output$label7 <- renderText({ labels()[7] })
  output$label8 <- renderText({ labels()[8] })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
