clique_by_naive_subsample <- function(G, k, p) {
  # Step 1
  m <- ecount(G)
  
  # Step 2
  vertex_probabilities <- degree(G)/(2*m) + p
  H <- sample_v(graph=G, size=vcount(G), prob=vertex_probabilities, replace=TRUE)
  
  # Step 3
  G_prime <- induced_subgraph(G, V(H))
  
  # Step 4
  top_k_vertices <- head(order(degree(G_prime), decreasing=TRUE), k)
  
  return(V(G_prime)[top_k_vertices])
}
