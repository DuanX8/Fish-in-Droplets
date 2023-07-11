  library(tiff)
  library(EBImage)
  
  library(tiff)
  
  ## raw images read and calculate the pixel difference between images
  
  xl<-list.files("file path..")
  for (h in 1:length(xl)) {
    x<-list.files(paste0("file path..",xl[h],"/"))
    BMP<-lapply(1:length(x), function(t){
      cat(t,"\n")
      dirs <- list.files(paste0("file path..",xl[h],"/",x[t]))
      bmp<-lapply(dirs,function(d) {
        files <- list.files(file.path(paste0("file path",xl[h],"/",x[t]), d))
        lapply(files, function(f) {
          files1 <- list.files(file.path(paste0("file path..",xl[h],"/",x[t]), d,f),pattern="\\.tif$")
          lapply(files1, function(e){
            tem<-readTIFF(file.path(paste0("file path..",xl[h],"/",x[t]), d, f,e))
            cat(file.path(paste0("file path..",xl[h],"/",x[t]), d, f,e),"\n")
            tem
            # })
          })
        })
      }) 
      CBM<-lapply(1:2, function(i){
        lapply(1:2, function(j){
          s<-length(bmp[[i]][[j]])
          D<-lapply(c((s*0.1):(s*0.9)), function(k){
            bmp[[i]][[j]][[k]]-bmp[[i]][[j]][[k-1]]
          })
          A<-matrix(0,dim(D[[1]])[1],dim(D[[1]])[2])
          for(m in 2:length(D)){
            A<-D[[m-1]]+A
          }
          Am<-A/length(D)
          Am 
        })
        
      })
      CBM
    })
    save(BMP,file = paste0("file path",".RData")) 
  }
  
  ## calculate the T score BAMs
  
  rawdat <- list()
  dirs <- list.files("file path..")
  for (d in dirs) {
    files <- list.files(file.path("file path..", d), pattern="\\.tif$")
    rawdat[[d]] <- lapply(files, function(f) {
      tem<-readTIFF(file.path("file path..", d, f))
    })
    names(rawdat[[d]])<-list.files(file.path("file path..", d))
    }

  m=239
  u=141
  
  difmat<-lapply(1:length(rawdat), function(i){
    k<-length(rawdat[[i]])
    diffmat<-array(NA, dim=c(m, u, k))
    cat(names(rawdat)[i],'\n')
    for (j in 1:k) {
      diffmat[,,j]<-resize(rawdat[[i]][[j]],m,u)
    }
    diffmat
  })

  tmatcal <- function(diffmat) {
    ##
    sdiffmat <- diffmat
    tmat <- tmat0 <- matrix(NA,dim(sdiffmat)[1],dim(sdiffmat)[2])
    for(i in 1:dim(diffmat)[1]){
      for(j in 1:dim(diffmat)[2]){
        x=diffmat[i,j,]
        xmean=mean(x)
        xsd=sd(x)
        tmat0[i,j]=xmean/xsd/sqrt(dim(sdiffmat)[3])
      }
    }
    tmat <- tmat0
    tmat
  }
  
  tmatv<- lapply(1:length(rawdat), function(i){
    cat(names(rawdat)[i],"\n")
    tma<-tmatcal(difmat[[i]])
    tma
  })
  
  names(tmatv)<-names(rawdat)
  
  
  

