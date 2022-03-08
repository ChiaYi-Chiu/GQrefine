GQrefine.auto = function(Y = Y, Q = Q, initial.att=NULL, distance = "whamming", ncyc=50){
  list = c("conj", "dis", "fixed")
  cand.Q = rss = NULL
  for (i in 1:3){
    ref = GQrefine(Y, Q, initial.att, distance, ideal = list[i], ncyc)
    rss = c(rss, ref$rss)
    cand.Q[[i]] = ref$modified.Q
  }
  min.rss = which.min(rss)
  modified.Q = cand.Q[[min.rss]]
  output = list(modified.Q = modified.Q, ideal = list[min.rss])
  
  return(output)
}
