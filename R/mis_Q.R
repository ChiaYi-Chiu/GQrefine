mis.Q = function(Q = Q, mis.p = 0.05){
  K=ncol(Q)
  J=nrow(Q)
  if (mis.p == 0) mis.Q = Q else{
    mis.Q = as.matrix(0)
    while (0%in%rowSums(mis.Q)==T){
      mis.q = sample(1:(J*K), sample(c(ceiling(J*K*mis.p), floor(J*K*mis.p)), 1))
      row = mis.q%%J
      while (0%in%row==T)
      {
        mis.q = sample(1:(J*K), sample(c(ceiling(J*K*mis.p), floor(J*K*mis.p)), 1))
        row = mis.q%%J
      }
      col = (mis.q-row)/J+1
      
      mis.Q = Q
      mis.Q[cbind(row, col)] = (1-mis.Q[cbind(row, col)])
    }
  }
  return(mis.Q)
}
