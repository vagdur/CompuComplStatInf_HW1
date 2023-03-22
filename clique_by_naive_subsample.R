clique_by_naive_subsample <- function(G, k, p, gamma) {
  # Step 1: Find probabilities of including each vertex in our subsample:
  vertex_percentiles <- rank(degree(G), ties.method="min")/vcount(G)
  vertex_probabilities <- (vertex_percentiles + p)**gamma
  
  # Step 2: Subsample
  H <- V(t$graph)[runif(length(V(t$graph))) < vertex_probabilities]
  
  # Step 3: Induced subgraph on our subsample
  G_prime <- induced_subgraph(G, H)
  
  # Step 4: Degree test on the subsampled graph
  top_k_vertices <- head(order(degree(G_prime), decreasing=TRUE), k)
  
  return(V(G_prime)[top_k_vertices])
}
