library(openxlsx)





# import excel data in to R
read_data<-function(input){
#  input<-'Example.xlsx'
  # extract gene names
  gene_list<-getSheetNames(input)
  all_data<-lapply(1:length(gene_list), function(x) openxlsx::read.xlsx(xlsxFile = input, sheet = x,colNames = TRUE))

  # change the list names to gene names
  names(all_data) <- gene_list
  # Add mean and sd
  for (m in 1:length(all_data)){
    sample_means<-NULL
    sample_sds<-NULL
    for (n in 1:length(sample_names)){
      sample_means<-c(sample_means, mean(all_data[[m]][,n],na.rm = TRUE))
      sample_sds<-c(sample_sds,sd(all_data[[m]][,n],na.rm = TRUE))
    }
    all_data[[m]]<-rbind(all_data[[m]],sample_means)
    all_data[[m]]<-rbind(all_data[[m]],sample_sds)
    rownames(all_data[[m]])<-c('repeat1','repeat2','repeat3','mean','sd')  
  }
  return(all_data)
}




