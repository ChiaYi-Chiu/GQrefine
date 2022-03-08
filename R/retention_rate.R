retention.rate = function(ref.Q = ref.Q, mis.Q = mis.Q, true.Q = true.Q)
{
  mis=mis.Q-true.Q
  rec=ref.Q-true.Q
  loc.cor=which(mis==0,arr.ind = TRUE)
  count=0
  for (m in 1:nrow(loc.cor)) if (rec[loc.cor[m,1],loc.cor[m,2]]==0) count=count+1
  ret.rate = count/nrow(loc.cor)
  return(ret.rate)
}
