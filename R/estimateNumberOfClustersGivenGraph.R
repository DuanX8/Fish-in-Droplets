
#' This function estimates the number of clusters using eigengap method.
#'
#' @param W  the similarity graph.
#' @param NUMC  a vector which contains the possible choices of number of clusters.
#' @return  K1 is the estimated best number of clusters according to eigen-gaps. K12 is the estimated SECOND best number of clusters according to eigen-gaps.
#' @export
#' @examples
#' data(GSE62254_data)
#' data1<-apply(GSE62254_gene.expression, 2, function(x) (x-mean(x))/sd(x))
#' A<-affs(data1)
#' [K1,K12]<-estimateNumberOfClustersGivenGraph(A,2:7);

estimateNumberOfClustersGivenGraph <- function(W, NUMC=2:7) {

  #   Note that this function can only give an estimate of the number of
  #   clusters. How to determine the "OPTIMAL" number of clusters, is still an
  #   open question so far.

  if (min(NUMC) == 1) {
    warning('Note that we always assume there are more than one cluster.');
    NUMC = NUMC[NUMC > 1]
  }

  W = (W + t(W))/2
  diag(W) = 0

  if (length(NUMC) > 0) {
    degs = rowSums(W)


    # compute unnormalized Laplacian

    degs[degs == 0] = .Machine$double.eps
    D = diag(degs)
    L = D - W
    Di = diag(1 / sqrt(degs))
    L = Di %*% L %*% Di

    # compute the eigenvectors corresponding to the k smallest
    # eigs$valuess
    eigs = eigen(L)
    eigs_order = sort(eigs$values, index.return=T)$ix
    eigs$values = eigs$values[eigs_order]
    eigs$vectors = eigs$vectors[, eigs_order]
    eigengap = abs(diff(eigs$values))
    eigengap = eigengap * (1 - eigs$values[1:length(eigs$values) - 1] ) / (1 - eigs$values[2:length(eigs$values)])

    quality = list()
    for (c_index in 1:length(NUMC)) {
      ck = NUMC[c_index]
      UU = eigs$vectors[, 1:ck]
      EigenvectorsDiscrete <- .discretisation(UU)[[1]]
      EigenVectors = EigenvectorsDiscrete^2

      # MATLAB: sort(EigenVectors,2, 'descend');
      temp1 <- EigenVectors[do.call(order, lapply(1:ncol(EigenVectors), function(i) EigenVectors[, i])), ]
      temp1 <- t(apply(temp1, 1, sort, TRUE))

      quality[[c_index]] = (1 - eigs$values[ck + 1]) / (1 - eigs$values[ck]) *
        sum( sum( diag(1 / (temp1[, 1] + .Machine$double.eps) ) %*% temp1[, 1:max(2, ck-1)] ))
    }

     t1 <- sort(eigengap[NUMC], decreasing=TRUE, index.return=T)$ix

    #ts1<-sort(eigengap[NUMC], decreasing=TRUE, index.return=T)
     K1 = NUMC[t1[1]]
     K12 = NUMC[t1[2]]
    #t2 <- sort(unlist(quality), index.return=TRUE)$ix
    #ts2 <- sort(unlist(quality), index.return=TRUE)
    #K2 <- NUMC[t2[1]]
    #K22 <- NUMC[t2[2]]
  }

  #return(list(ts1,ts2))
  return (list(K1,K12))
  #return (list(K1,K12, K2,K22))
}


.discretisationEigenVectorData <- function(eigenVector) {

  Y = matrix(0,nrow(eigenVector),ncol(eigenVector))
  maxi <- function(x) {
    i = which(x == max(x))
    return(i[1])
  }
  j = apply(eigenVector,1,maxi)
  Y[cbind(1:nrow(eigenVector),j)] = 1

  return(Y)

}


