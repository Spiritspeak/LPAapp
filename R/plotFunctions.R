plotMarginalDistribution <- function(parameters, dataset, variable){
  varname<-colnames(dataset)[variable]
  longData<- dataset[variable] %>% gather(key="var")
  
  sds <- data.frame(sqrt(apply(parameters$variance$sigma, 3, diag)))
  colnames(sds) <- paste0("group", 1:ncol(sds))
  sds$var <- rownames(sds)
  
  sds <- gather(sds, key = "group", value="sd", contains('group'))
  
  mu <- data.frame(parameters$mean)
  colnames(mu) <- paste0("group", 1:ncol(mu))
  mu$var <- rownames(mu)
  
  mu <- gather(mu, key = "group", value="mu", contains('group'))
  
  norm_curves <- merge(sds, mu)
  
  pro <- tibble(pro = parameters$pro, group = paste0("group", 1:length(parameters$pro)))
  
  minmax <- longData %>% group_by(var) %>% summarise(min = min(value), max = max(value))
  minmax$min <- minmax$min - 0.1*(minmax$max - minmax$min)
  minmax$max <- minmax$max + 0.1*(minmax$max - minmax$min)
  
  norm_curves <- merge(norm_curves, minmax)
  norm_curves <- merge(norm_curves, pro)
  
  plot_data <- pmap_df(norm_curves, function(group, var, sd, mu, min, max, pro){
    tibble(group = group, var = var, x = seq(min, max, length.out = 200), y = pro*dnorm(x, mu, sd))
  })
  
  plot_data$y_sum <- spread(plot_data, key = group, value = y) %>% select(contains('group')) %>% rowSums()
  
  
  finalplot<-ggplot(data = plot_data) +
    stat_bin(data = longData, aes(x = value, y = ..density..), alpha = 0.3,bins=15) + 
    geom_line(aes(group = group, x = x, y = y, col = group), alpha = 1, lwd = 1) + 
    geom_line(aes(x = x, y = y_sum), alpha = 0.3, lwd = 1) +
    theme_bw() + 
    annotation_custom(grobTree(textGrob(varname, x=0,  y=0.95, hjust=0,
                              gp=gpar(fontsize=13)))) +
    theme(legend.position="none",axis.title = element_blank(),axis.text=element_blank(), plot.margin = unit(c(0.1,0.1, 0.1, 0.1), "cm"))
  
  return(finalplot)
}

makecircleplot<- function(dataset,fit,xind,yind){
  classes<-as.factor(fit$classification)
  dataset2<-cbind(dataset,classes)
  circles<-list()
  for(value in unique(classes)){
    circleset<-dataset2[classes==value,]
    circles[[value]]<-geom_encircle(data=circleset,aes_string(x=colnames(circleset)[xind],y=colnames(circleset)[yind]))
  }
  gg<-ggplot(data=dataset2,aes_string(x=colnames(dataset2)[xind],y=colnames(dataset2)[yind],colour="classes")) + 
    geom_point(size = 1)+
    circles+
    theme_bw()+
    theme(legend.position="none",axis.title=element_blank(),axis.text=element_blank(),plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  return(gg)
}

ultrascatterplot<-function(dataset,classes,fit){
  #dataset<-as.matrix(fit$data)
  #classes<-as.factor(fit$classification)
  matsize<-ncol(dataset)
  plotmat<-matrix(ncol=matsize,nrow=matsize)
  plotlist<-list()
  
  for(i in 1:matsize){ #over matrix columns
    for(j in 1:matsize){ #over matrix rows
      if(i<j){
        plotlist[[(i-1)*matsize+j]]<-makecircleplot(dataset,fit,i,j)
        
      }else if(i==j){
        plotlist[[(i-1)*matsize+j]]<-plotMarginalDistribution(fit$parameters,dataset,i)
        
      }else if(j==1 && i==matsize){
        
        #dataset2<-cbind(dataset,classes)
        #plotlist[[(i-1)*matsize+j]]<-legendplot(fit)
      }else{
        plotlist[[(i-1)*matsize+j]]<-""
      }
    }
  }
  multiplot(plotlist,cols=matsize)
}

#Adapted from cookbook-r.com
multiplot <- function(plots, file, cols=1, layout=NULL) {
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

pieplot<-function(fit){ 
  classes<-fit$classification
  
  fillvect<- unique(classes)
  fillvect<-factor(fillvect,levels=fillvect)
  
  pie<-vector()
  groupid<-vector()
  grnr<-vector()
  for(i in 1:length(unique(classes))){
    grnr[i]<-i
    pie[i]<-sum(classes==i)
    groupid[i]<-paste0(i," (",pie[i],")")
  }
  pie<-data.frame(cbind(grnr,pie,groupid))
  colnames(pie)<-c("group","grsize","grname")
  pie$grsize<-as.numeric(as.vector(pie$grsize))
  niceplot<-ggplot(pie,aes(x="",y=grsize)) +
    scale_fill_discrete(name="Group",labels=pie$grname) +
    geom_bar(width=1,stat="identity",aes(fill=fillvect)) +
    coord_polar("y") + theme_bw() +
    theme(axis.title=element_blank(),
          panel.border=element_blank(),
          axis.text=element_blank(),
          text=element_text(size=20)
    ) + ggtitle("Group sizes")
    
  
  return(niceplot)
}
