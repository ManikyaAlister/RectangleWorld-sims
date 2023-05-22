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
                titlePanel("Heatmap Visualization"),
                
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
                
                
                # Sidebar layout
                sidebarLayout(
                  
                  # Sidebar panel
                  sidebarPanel(
                    
                    # Experiment selection
                    selectInput("experiment", "Experiment", choices = c(1, 2), selected = 1),
                    
                    # Block selection
                    selectInput("b", "Block", choices = c(1, 2, 3, 4, 5, 6, 7, 8), selected = 1),
                    
                    # Clue number selection
                    selectInput("clueNum", "Clue Number", choices = c(1, 2, 3, 4), selected = 1)
                    
                  ),
                  
                  # Main panel
                  mainPanel(
                    
                    # Heatmap plots and labels
                    fluidRow(
                      column(3, h3(textOutput("label1"), align = "center")),
                      column(3, h3(textOutput("label2"), align = "center")),
                      column(3, h3(textOutput("label3"), align = "center")),
                      column(3, h3(textOutput("label4"), align = "center"))
                    ),
                    fluidRow(
                      column(3, div(plotOutput("plot1"), class = "plot-outline")),
                      column(3, div(plotOutput("plot2"), class = "plot-outline")),
                      column(3, div(plotOutput("plot3"), class = "plot-outline")),
                      column(3, div(plotOutput("plot4"), class = "plot-outline"))
                    ),
                    fluidRow(
                      column(3, h3(textOutput("label5"), align = "center")),
                      column(3, h3(textOutput("label6"), align = "center")),
                      column(3, h3(textOutput("label7"), align = "center")),
                      column(3, h3(textOutput("label8"), align = "center"))
                    ),
                    fluidRow(
                      column(3, div(plotOutput("plot5"), class = "plot-outline")),
                      column(3, div(plotOutput("plot6"), class = "plot-outline")),
                      column(3, div(plotOutput("plot7"), class = "plot-outline")),
                      column(3, div(plotOutput("plot8"), class = "plot-outline"))
                    )
                    
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
  
  # Plot 1
  output$plot1 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "HS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 2
  output$plot2 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "RS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 3
  output$plot3 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "MS", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 4
  output$plot4 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "US", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 5
  output$plot5 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "HN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 6
  output$plot6 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "RN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 7
  output$plot7 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "MN", clueNum = input$clueNum, experiment = input$experiment)
  })
  
  # Plot 8
  output$plot8 <- renderPlot({
    plotShinyHeatMaps(d = d_cartesian(), b = input$b, condition = "UN", clueNum = input$clueNum, experiment = input$experiment)
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
