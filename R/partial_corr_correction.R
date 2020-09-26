#Performing partial correlation correction
parcorr<-function(corrmat, vals, group, comm_mats){
  corr_mat[[group]][!is.finite(corr_mats[[group]])] <- 0
  
  amat= corr_mats[[group]]
  zmat <- matrix(0, ncol = nrow(corr_mats[[group]]), nrow = nrow(corr_mats[[group]]))
  pmat <- matrix(0, ncol = nrow(corr_mats[[group]]), nrow = nrow(corr_mats[[group]]))
  n <- ncol(corr_mats[[group]])
  
  
  for (i in 1:nrow(corr_mats[[group]])) {
    for (j in i:nrow(corr_mats[[group]])) {
      zmat[i, j] <- atanh(corr_mats[[group]][i, j])
      zmat[j, i] <- zmat[i, j]
      pmat[i, j] <- 2 * pnorm(abs(zmat[i, j]), 0, sqrt(1/(n-3)), lower.tail=FALSE)
      pmat[j, i] <- pmat[i, j]
    }
  }
 
  n <- length(pmat)
  
  pcorrmat <- matrix(0, dim(amat)[1], dim(amat)[2])
  
  
  for(i in 1:nrow(amat)){ 
    for(j in 1:nrow(amat)){
      rowi <- amat[i, -c(i, j)]
      rowj <- amat[j, -c(i, j)] 
      tmp <- (amat[i, j] - rowi * rowj) / sqrt((1 - rowi^2) * (1 - rowj^2)) 
      tmp.zvals <- (0.5) * log((1 + tmp) / (1 - tmp)) 
      tmp.s.zvals <- sqrt(n - 1 - 3) * tmp.zvals 
      tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 0, 1, lower.tail = FALSE) 
      pcorrmat[i, j] <- max(tmp.pvals)
    } 
  }
  
  
  pcorradjmat_BH <- matrix(p.adjust(pcorrmat, method = "BH", n = length(pcorrmat)), nrow = nrow(corr_mats[[group]]), ncol = nrow(corr_mats[[group]]))
  pcorr_graph_BH <- matrix(as.numeric(pcorradjmat_BH < vals), nrow = nrow(corr_mats[[group]]), ncol = nrow(corr_mats[[group]]))
  comm_mats[[group]]=pcorr_graph_BH
  #comm_mats[[group]] = matrix(rmTest(rowMeans(bootstrap)),nrow = no_of_taxa,ncol = no_of_taxa)
  #comm_mats[[group]] <- ceiling(0.5 * (comm_mats[[group]] + t(comm_mats[[group]])))
}