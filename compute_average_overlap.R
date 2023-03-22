sample_overlaps <- function(find_clique, N, n, k, p, q, subsample_p = 0, subsample_gamma = 0) {
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
    
    # Find the clique using the find_clique function - passing extra params if they're nonzero
    if (subsample_gamma != 0 && subsample_p != 0) {
      S_hat <- find_clique(graph, k, subsample_p, subsample_gamma)  
    } else {
      S_hat <- find_clique(graph, k)
    }
    
    
    # Compute the overlap between S_hat and the actual set S
    overlap <- length(intersect(S, S_hat)) / k
    
    # Return the overlap:
    return(overlap)
  }, .options = furrr_options(seed = TRUE))
  
  # Return the vector of overlaps:
  return(overlaps)
}

#Doesn't work with the naive subsample method
plot_average_overlap <- function(find_clique, N, n, k, q) {
  p_values <- seq(0, 1, by = 0.1)
  overlaps <- sapply(p_values, function(p) {
    mean(sample_overlaps(find_clique, N, n, k, p, q))
  })
  data <- data.frame(p = p_values, overlap = overlaps)
  ggplot(data, aes(x = p, y = overlap)) +
    geom_line() +
    labs(x = "p", y = "Average Overlap")
}

# Define function to generate plot
plot_overlap_boxplot <- function(find_clique, N, n, k, q, subsample_p = 0, subsample_gamma = 0) {
  # Pick points to sample in:
  p_values <- seq(0, 1, by = 0.1)
  
  # Generate data frame with overlap samples for each p value
  overlap_df <- data.frame()
  for (p in p_values) {
    overlap_samples <- sample_overlaps(find_clique, N, n, k, p, q, subsample_p, subsample_gamma)
    overlap_df <- rbind(overlap_df, data.frame(p = p, overlap = overlap_samples))
  }
  
  # Create ggplot boxplot
  ggplot(overlap_df, aes(x = factor(p), y = overlap)) +
    geom_boxplot() +
    ggtitle("Overlap Spread by p") +
    xlab("p") +
    ylab("Overlap")
}

