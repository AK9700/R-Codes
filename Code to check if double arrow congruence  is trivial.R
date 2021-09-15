double.arrow.triv<-function(m){
  if(all.equal((rowSums(m!= 0) %ge% 2),rep(TRUE,nrow(m)))==TRUE){
    return(1)
  }
  return(0)
}