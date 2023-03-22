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
library(ggplot2)
library(furrr)

source("generate_planted_clique.R")
source("clique_by_top_degrees.R")
source("clique_by_spectrum.R")
source("clique_by_naive_subsample.R")
source("compute_average_overlap.R")

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
            sliderInput("N",
                        "Number of graphs to sample for overlap boxplot",
                        min = 1,
                        max = 100,
                        value = 1,
                        round = TRUE),
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

        # Show a plot of the generated graph
        mainPanel(
          tags$p("Vertices neither in the actual ground truth clique nor in the estimated clique are grey. Correctly estimated vertices are green. Type 1 errors (vertices falsely identified as being in the clique) are orange, type 2 errors (vertices in the clique but not found) are red."),
          plotOutput("graphPlot"),
          tags$p("The below plot illustrates how the overlap - the number of correctly found clique vertices divided by k - varies as we vary the probability of edges outside the clique, keeping all other parameters fixed to the values set in the sidebar."),
          plotOutput("overlapBoxplot")
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
    } else if (input$algorithm == "Naive subsample method") {
      clique <- clique_by_naive_subsample(g, input$k, input$subsample_p, input$subsample_gamma)
    }
    return(clique)
  })
  
  graph_vertex_colors <- reactive({
    g <- planted_clique_graph()$graph
    S_star <- planted_clique_graph()$S
    
    # Default colour is grey:
    vcolours <- rep("grey", length(V(g)))
    
    # Things in the actual clique but not found are red:
    vcolours[S_star] <- "red"
    
    # Things not in the clique the algorithm found are orange:
    vcolours[recovered_clique()] <- "orange"
      
    # Things in the intersection are green:
    vcolours[intersect(S_star, recovered_clique())] <- "green"
      
    vcolours
  })
  
  graph_layout <- reactive({
    layout_with_fr(planted_clique_graph()$graph)
  })
  
  output$graphPlot <- renderPlot({
    g <- planted_clique_graph()$graph
    V(g)$color <- graph_vertex_colors()
    plot.igraph(g, layout = graph_layout())
  })
  
  output$overlapBoxplot <- renderPlot({
    if (input$algorithm == "Degree test") {
      p <- plot_overlap_boxplot(clique_by_top_degrees, input$N, input$n, input$k, input$q)
    } else if (input$algorithm == "Spectral method") {
      p <- plot_overlap_boxplot(clique_by_naive_subsample, input$N, input$n, input$k, input$q)
    } else if (input$algorithm == "Naive subsample method") {
      p <- plot_overlap_boxplot(clique_by_top_degrees, 
                                input$N, input$n, input$k, input$q,
                                input$subsample_p, input$subsample_gamma)
    }
    plot(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
