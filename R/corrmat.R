#generating corrmats
get_corrmat<-function(group,Count_data){
#generate a count matrix for current group
mat = matrix(0,0,length(unique(Count_data$Taxon)))
#start and end depth of the group
start = ranges[group,1]
end = ranges[group,2]
#extract counts in chunks of 30cm
for(i in seq(start,(end-0.2),0.2)){
  r = rep(0,no_of_taxa); names(r) = unique(Count_data$Taxon)
  for(j in seq(i,i+0.1,0.1)){
    idx = which(depth == j)
    r[Count_data$Taxon[idx]] = r[Count_data$Taxon[idx]] + Count_data$count[idx]
  }
  mat = rbind(mat,r)
}
#generate pseudo-counts by random bootstrap process, 1000 iterations
bootstrap = sapply(seq(1000), function(i){
  null = t(apply(mat,1,function(x) {
    #obtain rl string for the stratum
    rlstr = c()
    new_count = rep(0,ncol(mat)); names(new_count) = unique(Count_data$Taxon)
    for(count in x){
      if(count > 0){
        idx = which(x == count)
        rlstr = c(rlstr,rep(unique(Count_data$Taxon)[idx],count))
      }
    }
    #sanity check: see if any fossils exist at this level
    if(length(rlstr) > 0){
      #perform a sample with replacement
      rlstr = sample(rlstr,replace = T)
      counts = table(rlstr)
      new_count[names(counts)] = counts
      return(as.numeric(new_count))
    }
    else{
      return(x)
    }
  }))
  #obtain fossil co-abundance adjacency matrix based on pseudo counts
  cc = suppressWarnings(cor(null))
  
  #set diagnals of the matrix to 0
  diag(cc) = 0
  
  #return adjacency matrix flattened as a vector
  return(as.vector(cc))
})
corr_mats[[group]] = matrix(rowMeans(bootstrap),nrow = no_of_taxa,ncol = no_of_taxa)
rownames(corr_mats[[group]])<- unique(Count_data$Taxon)
colnames(corr_mats[[group]])<- unique(Count_data$Taxon)

print(lattice::levelplot(corr_mats[[group]], scales=list(x = list(cex = .3,rot = 90),y = list(cex=.3)), at = seq(-1,1,0.1),colorkey = list(col = colorRampPalette(c("blue","white","red"))),col.regions = colorRampPalette(c("blue","white","red"))))
}