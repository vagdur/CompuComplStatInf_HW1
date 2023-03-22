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
            sliderInput("q",
                        "Probability of edges within clique: (q)",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput("p",
                        "Probability of edges not within clique: (p)",
                        min = 0,
                        max = 1,
                        value = 0.3),
            selectInput("algorithm",
                        "Algorithm to use to find clique",
                        c("Degree test", "Spectral method", "Naive subsample method"),
                        "Degree test"),
            conditionalPanel(
              condition = "input.algorithm == 'Naive subsample method'",
              tags$p("Our naive subsample method samples a random induced subgraph of G by including each vertex with probability (r_v + p)^gamma, where r_v is the percentile rank of v in the ordering of degrees."),
              sliderInput("subsample_p",
                          "Parameter p for subsampling:",
                          min = 0,
                          max = 1,
                          value = 0.3),
              sliderInput("subsample_gamma",
                          "Parameter gamma for subsampling:",
                          min = 0,
                          max = 10,
                          value = 2)
            )
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
  
  recovered_clique <- reactive({
    g <- planted_clique_graph()$graph
    if (input$algorithm == "Degree test") {
      clique <- clique_by_top_degrees(g, input$k)
    } else if (input$algorithm == "Spectral method") {
      clique <- clique_by_spectrum(g, input$k)
    } else if (input$algorthm == "Naive subsample method") {
      clique <- clique_by_naive_subsample(g, k, input$subsample_p, input$subsample_gamma)
    }
    return(clique)
  })
  
  output$graphPlot <- renderPlot({
      plot.igraph(planted_clique_graph()$graph)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
