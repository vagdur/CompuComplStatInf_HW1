#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)

source("generate_planted_clique.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Recovery of the planted clique"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of vertices:",
                        min = 1,
                        max = 100,
                        round = TRUE,
                        value = 30),
            sliderInput("k",
                        "Size of planted clique:",
                        min = 1,
                        max = 100,
                        round = TRUE,
                        value = 5),
            sliderInput("p",
                        "Probability of edges within clique:",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput("q",
                        "Probability of edges not within clique:",
                        min = 0,
                        max = 1,
                        value = 0.3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("graphPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  planted_clique_graph <- reactive(generate_planted_clique(input$n, input$k, input$p, input$q))
  
  output$graphPlot <- renderPlot({
      plot.igraph(planted_clique_graph()$graph)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
