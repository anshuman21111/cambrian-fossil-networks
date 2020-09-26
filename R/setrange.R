#get the group strata and running TF
#get depth in numeric format
depth = as.numeric(gsub("m","",Count_data$Depth))
#define depth groups as discussed A,B,C,D
ranges = matrix(anres(Count_data(data)),4,2)
rownames(ranges) = c("A","B","C","D")

runranges=matrix(seq(min(Count_data(data)$Depth, max(Count_data(data)$Depth)-1, by=0.2)))
#number of taxa in the dataset
no_of_taxa = length(unique(Count_data(data)$Taxon))
