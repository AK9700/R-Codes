binary3.2 <- function(m, n) {
  mn <- m*n
  perm <- RcppAlgos::permuteGeneral(0:1, mn, TRUE)
  l<-asplit(array(t(perm), c(m, n, 2^(m*n))), 3)
  for(i in 1:length(l)){
    for(j in 1:n){
      m<-l[[i]]
      if(all(m[j,]==rep(0,n))){
        l[[i]]<-0
        break
      }
    }
  }
for(i in length(l):1){
    if(identical(dim(l[[i]]),NULL)==TRUE){
      l[[i]]<-NULL
    }
  }
return(l)
}


