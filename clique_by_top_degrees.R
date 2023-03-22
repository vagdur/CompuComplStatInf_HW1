get_top_k_degrees <- function(graph, k) {
  # Get the degrees of all vertices in the graph
  degrees <- degree(graph)
  
  # Find the indices of the k vertices with highest degrees
  top_k_indices <- order(-degrees)[1:k]
  
  # Return the indices of the top k vertices
  return(top_k_indices)
}
