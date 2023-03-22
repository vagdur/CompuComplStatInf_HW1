clique_by_spectrum <- function(graph, k) {
  # Get the adjacency matrix of the graph
  adj_matrix <- as.matrix(get.adjacency(graph))
  # this returns a non-symmetric matrix using only the upper
  # half, so we need to make it symmetric:
  adj_matrix <- adj_matrix + t(adj_matrix)
  
  # Create the weight matrix W = 2A - 1 off the diagonal, 0 on the diagonal
  n <- nrow(adj_matrix)
  W <- 2 * adj_matrix - 1 + diag(n)
  
  # Compute the top eigenvector of W
  ev <- eigen(W)$vectors[,1]
  
  # This top eigenvector will have the type "complex" despite of course consisting
  # of reals, so we fix this:
  ev <- as.double(ev)
  
  # Get the indices of the k largest values in the eigenvector
  K_tilde <- order(ev, decreasing = TRUE)[1:k]
  
  # Get the indices of the vertices in g having at least 3k/4 neighbours in K_tilde
  K_hat <- which(rowSums(adj_matrix[, K_tilde]) >= 0.75 * k)
  
  # Return K_hat
  return(K_hat)
}
