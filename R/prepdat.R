#data preprocessing, setting data types in ab file
#data is in "~/abundance_data.csv"
Count_data<-function(data){
  Count_data = read.csv(data)
  print(table(Count_data$Depth))
  print("Stratifications")
  print(table(Count_data$Depth))
  Count_data$Taxon= as.character(Count_data$Taxon)
  Count_data$Depth= as.numeric(Count_data$Depth)
  Count_data$Section=as.numeric(Count_data$Section)
}
