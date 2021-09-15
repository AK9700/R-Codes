#You may want to install the following
install.packages("remotes")
remotes::install_github("hangxiong/wNetwork")
library(wNetwork)

vert.to.edge<-function(m_2){
  wNetwork::adjacency.to.edgelist(m_2)->edge
  
  colnames(edge)=NULL
  
  len_1<-length(edge)/2
  
  matrix(0, nrow = len_1, ncol = len_1)->m
  
  for(i in 1:len_1){
    edge[i,2]->b
    for(j in 1:len_1){
      if(edge[j,1]==b){
        m[i,j]=1
      }
    }
  }
  return(m)
}