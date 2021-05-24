

showBIC<-function(fit){
  mat<-matrix(fit$BIC,ncol=length(attr(fit$BIC,"modelNames")))
  colnames(mat)<-attr(fit$BIC,"modelNames")
  rownames(mat)<-attr(fit$BIC,"G")
  mat<-round(mat,2)
  #mat2<-mat[-which(mat==fit$bic)]
  
  brks <- quantile(mat,na.rm=TRUE,probs=seq(.05,.95,.05))

  brks[which(brks==round(fit$bic,2))]<-fit$bic-1
  brks[20]<-fit$bic-0.1
  #print(brks)
  
  clrs <- round(seq(127, 255, length.out = length(brks) + 1), 0) %>%
  paste0("rgb(255,", ., ",", ., ")")
  clrs[21]<-'rgb(255,255,127)'
  
  select <- which(mat == max(mat,na.rm = TRUE), arr.ind = TRUE)
  #select <- list(row = select[1,1], column = select[1,2])
  print(select)
  #select <- list(row = 1, column = 1)
  return(DT::datatable(mat, class = 'cell-border stripe',
         selection=list(mode="single", target="cell", selected=select),
         options=list(scrollX=F)
         ) %>% formatStyle(columns=colnames(mat), backgroundColor = styleInterval(brks,clrs)))
}

showGroupMeans<-function(fit){
  #Row: groups. Column: variables.
  #Displayed stats: means (SD)
  #Group name has (n=X) next to it
# 
#   meanmat<-round(as.matrix(t((fit$parameters)$mean)),2)
#   varmats<-fit$parameters$variance$orientation
#   classvect<-fit$classification
#   ngroup=length(unique(classvect))
#   nvar=nrow(varmats)
#   sdmat<-matrix(ncol=ngroup,nrow=nvar)
#   
#   #Figure out how many people are in each class
#   sizevector<-vector() 
#   
#   namevect<-vector()
#   for(i in 1:ngroup){
#     sizevector[i]<-length(classvect[classvect==unique(classvect)[i]])
#     namevect[i]<-paste("Group ",i," (N=",sizevector[i],")",sep="")
#     sdmat[,i]<-varmats[,,i][1==diag(nrow(varmats[,,i]))]
#   }
#   
#   sdmat<-round(t(sdmat),2)
#   combinearray<-array(c(meanmat,sdmat),dim=c(ngroup,nvar,2))
#   
#   combineMeanSD<-function(X){
#     X<-paste(X[1]," (",X[2],")",sep="")
#     return(X)
#   }
#   finalmat<-apply(combinearray,MARGIN=c(1,2),FUN=combineMeanSD)
#   rownames(finalmat)<-namevect
#   colnames(finalmat)<-colnames(varmats)


  # classvect <- fit$classification
  # meanmat<-round(as.matrix(t((fit$parameters)$mean)),2)
  # varmats<-fit$parameters$variance$orientation
  # 
  # ngroup=length(unique(classvect))
  # nvar=nrow(varmats)
  # sdmat<-matrix(ncol=ngroup,nrow=nvar)
  # 
  # #Figure out how many people are in each class
  # sizevector<-vector()
  # 
  # namevect<-vector()
  # for(i in 1:ngroup){
  #   sizevector[i]<-length(classvect[classvect==unique(classvect)[i]])
  #   namevect[i]<-paste("Group ",i," (N=",sizevector[i],")",sep="")
  #   sdmat[,i]<-varmats[,,i][1==diag(nrow(varmats[,,i]))]
  # }nround(t(sdmat),2)
  # combinearray<-array(c(meanmat,sdmat),dim=c(ngroup,nvar,2))
  # 
  # combineMeanSD<-function(X){
  #   #X<-paste(X[1]," (",X[2],")",sep="")
  #   return(X[1])
  # }
  # finalmat<-apply(combinearray,MARGIN=c(1,2),FUN=combineMeanSD)
  # 
  # finalmat <- as.matrix(finalmat)
  # rownames(finalmat)<-namevect
  # colnames(finalmat)<-colnames(varmats)
  
  finalmat <- as.matrix(fit$parameters$mean)
  finalmat <- round(finalmat, 3)
  colnames(finalmat) <- paste("Group", 1:ncol(finalmat))
  return(DT::datatable(finalmat))
}


showGroupVariances <-function(fit){
  #Row: groups. Column: variables.
  #Displayed stats: means (SD)
  #Group name has (n=X) next to it
 
  finalmat <- apply(fit$parameters$variance$sigma, 3, function(x) diag(x))
  finalmat <- round(finalmat, 3)
  colnames(finalmat) <- paste("Group", 1:ncol(finalmat))
  return(DT::datatable(finalmat))
}

showGroupMembership <- function(fit, data){
  return(cbind(data, Classification = fit$classification, Uncertainty = fit$uncertainty))
}
#datatable(showGroupStats(fit))
#extract variances




