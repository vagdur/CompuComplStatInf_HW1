sample_overlaps <- function(find_clique, N, n, k, p, q) {
  # Initialize total overlap to 0
  total_overlap <- 0
  
  # Set up to parallelize:
  plan("multisession")
  
  # Generate N random graphs and compute overlap for each graph
  overlaps <- future_map_dbl(1:N, function(unused_argument) {
    # Generate a random graph using the generate_graph function
    graph_and_S <- generate_planted_clique(n, k, p, q)
    graph <- graph_and_S[[1]]
    S <- graph_and_S[[2]]
    
    # Find the clique using the find_clique function
    S_hat <- find_clique(graph, k)
    
    # Compute the overlap between S_hat and the actual set S
    overlap <- length(intersect(S, S_hat)) / k
    
    # Return the overlap:
    return(overlap)
  }, .options = furrr_options(seed = TRUE))
  
  # Return the vector of overlaps:
  return(overlaps)
}
  
  # Compute the average overlap among the graphs
  avg_overlap <- total_overlap / N
  
  # Return the average overlap
  return(avg_overlap)
}
