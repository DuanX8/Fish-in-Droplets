 
  ## PCA

  ##data is a list of  T-score Bams 
  ptmat <- sapply(1:length(data), function(x) as.numeric(data[[x]]))
  ptmat <- t(ptmat)
  ptmat[which(is.na(ptmat))] <- 0
  row.names(ptmat)<-names(data)
  # ptmat[which(!is.finite(ptmat))] <- 0
  pca <- prcomp(ptmat)
  pc.use <- 20
  featmat4 <- pca$x[, 1:pc.use]
  
  set.seed(123)
  
  af<-affs(featmat4)
  
  sfd<-self.diffusion(af,4)
  cor.mat <- cor(t(sfd))
  fet<-as.dist(1-cor.mat)
  
  
  
  ress<-ConsensusClusterPlus(fet,maxK=10,reps=1000,pFeature=1,innerLinkage="average", finalLinkage="average",
                             clusterAlg="km",distance="pearson",seed=1111)
  
  
  lab<-ress[[4]]$consensusClass
  
  
  