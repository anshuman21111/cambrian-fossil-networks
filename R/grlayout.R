#graph layout
graph_m<-function(comm_mats, group, graph_list_sim, graph_list_org){
  graphRM <- graph_from_adjacency_matrix(comm_mats[[group]], mode = "undirected", weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)
  graph_list_org[[group]]=graphRM
  
  #simpify graph, remove errant vertices
  graphRM <- delete_vertices(simplify(graphRM), degree(graphRM)==0)
  
  #make graph plot simpler to follow
  V(graphRM)$frame.color <- "white"
  V(graphRM)$label <- ""
  V(graphRM)$color <- ""
  V(graphRM)$size <- 10
  E(graphRM)$arrow.mode <- 0
  
  #export
  graph_list_sim[[group]]=graphRM
  
  #get coordinates
  coordsRM = layout_with_fr(graphRM)
}