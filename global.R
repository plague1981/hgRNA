library(openxlsx)
library(ggplot2)
library(reshape2)



# import excel data in to R
read_data<-function(input){
  # extract gene names
  #input<-'Example.xlsx'
  gene_list<-getSheetNames(input)
  all_data<-lapply(1:length(gene_list), function(x) openxlsx::read.xlsx(xlsxFile = input, sheet = x,colNames = TRUE))
  sample_names<-colnames(all_data[[1]])
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


ddCt_table<-function(ave_dCt_table, samples){
  dCt_list<-ave_dCt_table[,samples]
  ddCt<-NULL
  exprs<-NULL
  if (is.null(samples)){
    NULL
  } else
    for (n in 1:length(dCt_list)){
      ddCt<-c(ddCt,as.numeric(dCt_list[n])-mean(as.numeric(ave_dCt_table[,input$control_samples])))
      exprs<-2^(-ddCt)
    }
  ddCt_list<-data.frame(ddCt)
  ddCt_list<-cbind(ddCt_list,exprs)
  ddCt_list<-data.frame(t(ddCt_list))
  colnames(ddCt_list)<-samples
  return(ddCt_list)
}

ddCt_table<-function(ave_dCt_table, samples){
  dCt_list<-ave_dCt_table[,samples]
  ddCt<-NULL
  exprs<-NULL
  if (is.null(samples)){
    NULL
  } else
    for (n in 1:length(dCt_list)){
      ddCt<-c(ddCt,as.numeric(dCt_list[n])-mean(as.numeric(ave_dCt_table[,input$control_samples])))
      exprs<-2^(-ddCt)
    }
  ddCt_list<-data.frame(ddCt)
  ddCt_list<-cbind(ddCt_list,exprs)
  ddCt_list<-data.frame(t(ddCt_list))
  colnames(ddCt_list)<-samples
  return(ddCt_list)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

exprs_table <- function(all_data,control_gene,target_gene,control_samples,sample_samples){
  ave_dCt_table<-all_data[[target_gene]]['mean',]-all_data[[control_gene]]['mean',]
  control_value<-ddCt_table(ave_dCt_table,control_samples)
  sample_value<-ddCt_table(ave_dCt_table,sample_samples)
  df<-t(data.frame(control_value['exprs',],sample_value['exprs',]))
  gp<-c(rep('control',length(control_value['exprs',])),rep('sample', length(sample_value['exprs',])))
  df<-data.frame(cbind(df,gp))
  return(df)
}

express_table <- function(df){
  exprs_mean <- aggregate(as.numeric(df[,'exprs']), by=list(df$gp), FUN=mean)
  exprs_sd <- aggregate(as.numeric(df[,'exprs']), by=list(df$gp), FUN=sd)
  ds<-merge(exprs_mean,exprs_sd,by = 'Group.1')
  colnames(ds)<-c('Group','Mean','SD')
  return(ds)
}

point_plot <- function (df){
  p<-ggplot2::ggplot(df, aes(x = gp, y = as.numeric(exprs)))+ 
    # Figure title
    ggtitle(input$plot_title) +
    # x axis title
    xlab(input$x_axis) +
    # y axis title
    ylab(input$y_axis) +
    # white background
    theme_classic() +
    geom_point(position = 'jitter',aes(shape=gp, color=gp), size=5 )+ 
    scale_shape_manual(values = c(input$shape_control,input$shape_sample)) +
    scale_color_manual(values=c(input$color_control,input$color_sample)) +
    # error bar
    stat_summary(fun.data=data_summary, geom='errorbar', color="red",width=.1)
#  + stat_summary(fun.data=data_summary, geom='pointrange', color="red")
  return(p)
}
#input<-'Example.xlsx'
#gene_list<-getSheetNames(input)
#all_data<-lapply(1:length(gene_list), function(x) openxlsx::read.xlsx(xlsxFile = input, sheet = x,colNames = TRUE))

# change the list names to gene names
#names(all_data) <- gene_list
# Add mean and sd
#for (m in 1:length(all_data)){
#  sample_means<-NULL
#  sample_sds<-NULL
#  for (n in 1:length(sample_names)){
#    sample_means<-c(sample_means, mean(all_data[[m]][,n],na.rm = TRUE))
#    sample_sds<-c(sample_sds,sd(all_data[[m]][,n],na.rm = TRUE))
#  }
#  all_data[[m]]<-rbind(all_data[[m]],sample_means)
#  all_data[[m]]<-rbind(all_data[[m]],sample_sds)
#  rownames(all_data[[m]])<-c('repeat1','repeat2','repeat3','mean','sd')
#}

#control_gene<-'HPRT'
#target_gene<-'hgRNA_1'

#ave_dCt_table<-all_data[[target_gene]]['mean',]-all_data[[control_gene]]['mean',]

#control_samples<-c('WT.cSA020m1','cSA014m5','cSA016m2','cSA016m3','cSA013m4')
#sample_samples<-c('cSA017m4','cSA018m2','cSA018m3','cSA018m4','cSA012m3')
#Mean_dCt<-as.numeric(mean(as.numeric(ave_dCt_table[,control_samples])))




#control_value<-ddCt_table(ave_dCt_table,control_samples)
#sample_value<-ddCt_table(ave_dCt_table,sample_samples)

#df<-t(data.frame(control_value['exprs',],sample_value['exprs',]))
#gp<-c(rep('control',length(control_value['exprs',])),rep('sample', length(sample_value['exprs',])))
#df<-data.frame(cbind(df,gp))

#exprs_mean <- aggregate(as.numeric(df[,'exprs']), by=list(df$gp), FUN=mean)
#exprs_sd <- aggregate(as.numeric(df[,'exprs']), by=list(df$gp), FUN=sd)
#ds<-merge(exprs_mean,exprs_sd,by = 'Group.1')
#colnames(ds)<-c('Group','Mean','SD')

#ggplot2::ggplot(df, aes(x = gp, y = as.numeric(exprs)))+ ggtitle('hgRNA expression') +xlab('groups') +ylab('Relative Expression') +
# geom_point(position = 'jitter',aes(shape=gp, color=gp),size=5)+ scale_shape_manual(values = c(15,16)) +
#  stat_summary(fun.data=data_summary, geom='errorbar', color="red",width=.1)+
# stat_summary(fun.data=data_summary, geom='pointrange', color="red")


