find.Ideal=function(Q = Q, weight = NULL){
  K=ncol(Q)
  J=nrow(Q)
  M=2^K
  pattern <- diag(K)
  for (l in 2:K){
    pattern <- rbind(pattern, t(apply(combn(K,l), 2, function(x){apply(pattern[x,], 2, sum)})))
  }
  pattern <- rbind(0, pattern)
  #=======Ideal.Conjuctive======#
  Ideal=pattern%*%t(Q) #M*K %*% K*J = M * J
  Ideal.conj=1*(Ideal==(matrix(1,M)%*%t(rowSums(Q))))
  #=======Ideal.Disjunctive=====#
  Ideal.dis=1*(Ideal>=1)
  #=======Ideal. mix=================#
  
  if (is.null(weight)){
    weight=Ideal/matrix(rep(colSums(t(Q)), M), M, J, T)
  } else {
    if (prod(dim(weight)==c(M,J))==0) print("Dimensions of weight is incorrect!")
  }
  
  Ideal.mix=Ideal.conj+(Ideal.dis-Ideal.conj)*weight
  return(list(conj=Ideal.conj, dis=Ideal.dis, mix=Ideal.mix))
}
