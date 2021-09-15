n.oid.brac<-function(n){
  for(i in 1:n){
    assign(paste0("k.",i),rep(0,n^2))
  }
  
  for(i in 1:n){
    for(j in 1:n^2){
      if(j > (i-1)*n && j<=i*n){
        m<-get(paste0("k.",i))
        m[j]<-1
        assign(paste0("k.",i),m)
      }
    }
  }
  
  r<-NULL
  
  for(i in 1:n^2){
    m<-i%%n
    if(m==0){
      m<-n
    }
    r<-rbind(r,get(paste0("k.",m)))
  }
  return(r)
}