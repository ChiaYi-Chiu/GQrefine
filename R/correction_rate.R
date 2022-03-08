correction.rate = function(ref.Q = ref.Q, mis.Q = mis.Q, true.Q = true.Q)
{
  if (sum(abs(mis.Q - true.Q))==0) cor.rate = "NA" else
  {
    mis=mis.Q-true.Q
    rec=ref.Q-true.Q
    loc.miss=which(mis!=0,arr.ind = TRUE)
    count=0
    for (m in 1:nrow(loc.miss)) if (rec[loc.miss[m,1],loc.miss[m,2]]==0) count=count+1
    cor.rate = count/nrow(loc.miss)
    return(cor.rate)
  }
}