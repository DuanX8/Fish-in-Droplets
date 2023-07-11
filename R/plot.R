  
  ## plot dendrogram
  library(dplyr)
  library(dendextend)
  
  dend15<-ress[[4]]$consensusTree
  dend15<-dend15%>% as.dendrogram
  #dend15$labels<-ress[[4]]$consensusClass
  dend15 %>% labels
  dend15 %>% set("labels", nam$X.1) %>% labels
  dend15_2 <- dend15 %>% 
    set("labels", nam$X.1) %>%    # change labels
    set("labels_col", c("#D62728FF","#1F77B4FF","#2CA02CFF","#FF7F0EFF"),k=4) %>%  # change color 
    set("labels_cex", 0.6)%>%set("branches_k_color", value = c("#D62728FF","#1F77B4FF","#2CA02CFF","#FF7F0EFF"), k = 4) 
  
  dend15_2 %>% plot(main = "")


  ##plot brain activity map
  
  pdf(file = "file/.pdf",width,height )
 
  for (i in 1:length(data)) {
    cat(i,"\n")
    r<-data[[i]]
    r[r>1]<-1
    r[r< (-1)]<- -1
    image(t(r[nrow(r):1, ]), axes=F, breaks=c(-3, seq(-1, 1, by=0.01), 3), useRaster=T,
          col=colorRampPalette(c("#c9e7f7","white","#fd7265"))(202),main=names(tmat_tem_comv)[i])
    
  }
  
  dev.off()
  
  
  ## plot network
  
  library(igraph)
  library(RedeR)
  rdp<-RedPort()
  calld(rdp)
  #axfu<- network.diffusion(axn,i)
  axfu<-sfd
  row.names(axfu)<-paste0("n",c(1:nrow(axfu)))
  colnames(axfu)<-paste0("n",c(1:nrow(axfu)))
  lab<-con_lab4
  names(lab)<-row.names(axfu)
  dl<-axfu
  dl<- log2(1+dl)
  dl[dl<0.031]<-0

  gmx<-graph.adjacency(dl,mode="undirected",weighted=TRUE,diag=FALSE)
  V(gmx)[names(lab[which(lab==1)])]$color  <-Var1[1]
  V(gmx)[names(lab[which(lab==2)])]$color  <-Var1[2]
  V(gmx)[names(lab[which(lab==3)])]$color  <-Var1[3]
  V(gmx)[names(lab[which(lab==4)])]$color  <-Var1[4]
  V(gmx)$size<- 18
  addGraph(rdp, gmx)
  
  relax(rdp)
  
  resetd(rdp)