#stochatic block model
library(mixer)
sbm_vals<-function(graphRM, meta, troint){
  
  meta.ordered <- meta[match(vertex_attr(graphRM)[["name"]], meta$NAME),]
  
  for (name in colnames(meta)){
    graphRM <- set_vertex_attr(graphRM, name, index = V(graphRM), meta[,name])
  }
  
  troint.sbm<- mixer(as.matrix(get.adjacency(graphRM)), qmin=3, qmax=8)
  
  troint.sbm.output <- getModel(troint.sbm)
  
  troint[[group]] = troint.sbm.output
  troint.sbm.output$q
  
  plot(troint.sbm, frame=1)
  plot(troint.sbm, frame=2)
  plot(troint.sbm, frame=3)
  plot(troint.sbm, classes=as.factor(V(graphRM)$HABITAT), frame=4, classes.col = unique(as.factor(V(graphRM)$HABITAT)))
  legend("bottomright", legend=levels(as.factor(V(graphRM)$HABITAT)), pch=c(16,16),col=unique(as.factor(V(graphRM)$HABITAT)), cex=0.8, text.font=4, bg='white', title = "Habitat")
  plot(troint.sbm, classes=as.factor(V(graphRM)$PHYLUM), frame=4, classes.col = unique(as.factor(V(graphRM)$PHYLUM)))
  legend("bottomright", legend=levels(as.factor(V(graphRM)$PHYLUM)), pch=c(16,16),col=unique(as.factor(V(graphRM)$PHYLUM)), cex=0.8, text.font=4, bg='white', title = "Phylum")
  plot(troint.sbm, classes=as.factor(V(graphRM)$FEED1), frame=4, classes.col = unique(as.factor(V(graphRM)$FEED1)))
  legend("bottomright", legend=levels(as.factor(V(graphRM)$FEED1)), pch=c(16,16),col=unique(as.factor(V(graphRM)$FEED1)), cex=0.8, text.font=4, bg='white', title = "Feeding Habits")
  plot(troint.sbm, classes=as.factor(V(graphRM)$MODE_MOV), frame=4, classes.col = unique(as.factor(V(graphRM)$MODE_MOV)))
  legend("bottomright", legend=levels(as.factor(V(graphRM)$MODE_MOV)), pch=c(16,16),col=unique(as.factor(V(graphRM)$MODE_MOV)), cex=0.8, text.font=4, bg='white', title = "Feeding Habits")
}
