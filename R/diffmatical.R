

diffmatcal<-function(x, m=w, u=h, k=5){
  summat1<-array(NA, dim=c(m, u, k))
  summat2<-array(NA, dim=c(m, u, k))
  diffmat<-array(NA, dim=c(m, u, k))
  for(i in 1:k){
    a=paste(i, "_test_", 0, ".mat", sep="")
    b=paste(i, "_test_", 1, ".mat", sep="")
    
    summat1[,,i]=x[[b]]+x[[a]]
    
    c=paste(i, "_control_", 0, ".mat", sep="")
    d=paste(i, "_control_", 1, ".mat", sep="")
    summat2[,,i]=x[[c]]+x[[d]]
    
    diffmat[,,i]=summat1[,,i] - summat2[,,i]
  }
  diffmat
}
