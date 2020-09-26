#general community detection
library(igraph)
gen_communities_plot<-function(graphRM){
  coordsRM = layout_with_fr(graphRM)
  print("Random Walk:")
  communities =  graphRM %>% 
    cluster_walktrap(weights = NULL, steps = 4,
                     merges = TRUE, modularity = TRUE, membership = TRUE)
  plot(communities, graphRM, layout=coordsRM)
  print("Betweenness:")
  communities = graphRM %>% 
    cluster_edge_betweenness(weights = NULL, directed = FALSE,
                             edge.betweenness = TRUE, merges = TRUE, bridges = TRUE,
                             modularity = TRUE, membership = TRUE)
  plot(communities, graphRM, layout=coordsRM)
  #print("Modularity:")
  #communities = graphRM %>% 
  #  cluster_optimal(weights = NULL)
  #plot(communities, graphRM, layout=coordsRM)
  #print((communities))
  print("Louvain:")
  communities = graphRM %>% 
    cluster_louvain(weights = NULL)
  plot(communities, graphRM, layout=coordsRM)
  print("Eigen:")
  communities = graphRM %>% 
    cluster_leading_eigen(steps = -1, weights = NULL, start = NULL,
                          options = arpack_defaults, callback = NULL, extra = NULL,
                          env = parent.frame())
  plot(communities, graphRM, layout=coordsRM)
  print("Greedy:")
  communities = graphRM %>% 
    cluster_fast_greedy(merges = TRUE, modularity = TRUE,
                        membership = TRUE)
  plot(communities, graphRM, layout=coordsRM)
}