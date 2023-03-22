compute_average_overlap <- function(find_clique, N, n, k, p, q) {
  # Initialize total overlap to 0
  total_overlap <- 0
  
  # Generate N random graphs and compute overlap for each graph
  for (i in 1:N) {
    # Generate a random graph using the generate_graph function
    graph_and_S <- generate_planted_clique(n, k, p, q)
    graph <- graph_and_S[[1]]
    S <- graph_and_S[[2]]
    
    # Find the clique using the find_clique function
    S_hat <- find_clique(graph, k)
    
    # Compute the overlap between S_hat and the actual set S
    overlap <- length(intersect(S, S_hat)) / k
    
    # Add the overlap to the total
    total_overlap <- total_overlap + overlap
  }
  
  # Compute the average overlap among the graphs
  avg_overlap <- total_overlap / N
  
  # Return the average overlap
  return(avg_overlap)
}
