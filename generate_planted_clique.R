library(igraph)

generate_planted_clique_graph <- function(n, k, p, q) {
  # Randomly sample a set S of k indices
  S <- sample(1:n, k)
  
  # Create an empty graph with n vertices
  g <- make_empty_graph(n)
  
  # Add edges between pairs of vertices in S with probability q
  S_pairs <- combn(S, 2)
  for (i in 1:ncol(S_pairs)) {
    if (runif(1) < q) {
      g <- add_edges(g, S_pairs[, i])
    }
  }
  
  # Add edges between pairs of vertices not both in S with probability p
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if ((i %in% S) != (j %in% S) && runif(1) < p) {
        g <- add_edges(g, c(i, j))
      }
    }
  }
  
  # Return the graph and the set S
  return(list(graph = g, S = S))
}
