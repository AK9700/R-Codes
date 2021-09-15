dclass<-function(m){
  m_1<-m
  for(i in 1:nrow(m)){
    for(j in 1:nrow(m)){
      m_1<-rbind(m_1,m[i,]*m[j,])
    }
  }
  d<-rep(0,nrow(m_1))
  for(i in 1:nrow(m_1)){
    if(all.equal(m_1[i,],rep(0,ncol(m_1)))==TRUE){
      d[i]<-1
    }
  }
  
  return(nrow(unique.matrix(m_1[rowSums(m_1[])>0,])))
}


