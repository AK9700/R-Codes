is.monoid<-function(m){
  for(i in 1:nrow(m))
    if(all.equal(m[i,],rep(1,nrow(m)))==TRUE){
      return(1)
    }
  return(0)
}