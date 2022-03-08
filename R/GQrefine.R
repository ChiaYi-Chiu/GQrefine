GQrefine=function(Y = Y, Q = Q, initial.att = NULL, distance = "whamming", ideal = c("conj", "dis", "fixed"), ncyc = 50){
  Y=as.matrix(Y)
  Q=as.matrix(Q)
  K=ncol(Q);J=nrow(Q);N=nrow(Y);M=2^K
  
  pattern <- diag(K)
  for (l in 2:K){
    pattern <- rbind(pattern,t(apply(combn(K,l),2,function(x){apply(pattern[x,],2,sum)})))
  }
  pattern <- rbind(0,pattern)
  pattern.q=pattern[-1,]
  
  initial.Q=Q
  index=NULL
  RSS=NULL
  diff.RSS=10
  
  q.min.rss.log=list()
  q.min.rss.log.jj=list()
  #q.max.rto.log=NULL
  for (cyc in 1:ncyc){ # Start cycle
    #q.max.rto.log.jj=NULL
    if(is.null(initial.att)){ # If alpha_i not given, then GNPC is applied
      classification=GNPC(Y, initial.Q, initial.gate = "AND", initial.dis = "Weighted")
      est.class=classification$class
      est.weight=classification$weight
    } else {
      est.class = NULL
      for (i in 1:N){
        c = rowSums(abs(matrix(rep(initial.att[i,],2^K),2^K, K) - pattern))
        est.class = c(est.class, which(c==0))
      }
    }
    if (cyc==1) initial.class=est.class
    
    if (ideal=="fixed"){
      est.ideal=find.Ideal(initial.Q)$mix[est.class,]
    } else if (ideal=="conj") {
      est.ideal=find.Ideal(initial.Q)$conj[est.class,]
    } else if (ideal=="disj"){
      est.ideal=find.Ideal(initial.Q)$dis[est.class,]
    } else {
      print("Ideal response not properly defined")
    }
    
    diff=Y-est.ideal
    rss=apply(diff^2,2,sum)
    names(rss)<-NULL
    RSS=rbind(RSS, c(rss, sum(rss)))
    #=========================================
    #            Begin Validation
    #=========================================
    rss.sum=0
    for (jj in 1:J){ # For each item
      
      Q.tmp=initial.Q #J*K
      update.rss=update.btn=update.rto=NULL 
      
      for (qq in 1:nrow(pattern.q)){
        #print(paste0("Loop:",cyc,", item: ",jj,", q.pattern: ",qq))
        Q.tmp[jj,]=pattern.q[qq,]
        u=find.Ideal(Q.tmp)$mix[est.class,]
        temp.rss=sum((Y[,jj]-u[,jj])^2)
        update.rss=c(update.rss, temp.rss)
      } # end of update rss, btn and rto
      min.update.rss=which.min(update.rss)
      update.q=pattern.q[min.update.rss,]
      initial.Q[jj,]=update.q
      rss.sum = rss.sum + min(update.rss)
    } #end jj loop
    #q.max.rto.log=rbind(q.max.rto.log, q.max.rto.log.jj)  
    q.min.rss.log[[cyc]]=q.min.rss.log.jj
    ##==========================================================================================##
    ## Stopping criterion: if all the J RSSs between two iterations are identical (which means  ##
    ## it's not possible to improve), break the loop.                                           ##
    ##==========================================================================================##
    if (cyc!=1 & is.null(initial.att)){
      if (sum(abs(RSS[cyc,]-RSS[cyc-1,]))==0) break
    } else if (is.null(initial.att)==F) {
      break
    }
    #     if (m!=1) {diff.RSS=RSS[nrow(RSS),J+1]-RSS[nrow(RSS)-1,J+1]
    #     if(diff.RSS==0) break}
  } # end cyc
  
  ##=============================##
  ## Report the modified entries ##
  ##=============================##
  if (sum((Q-initial.Q)^2)==0) modified="NA" else 
  {
    modified=which(Q-initial.Q!=0, arr.ind=TRUE)
    colnames(modified)=c("Item","Skill")
  }
  output=list(initial.class=initial.class, 
    terminal.class=est.class, 
    modified.Q=initial.Q, 
    modified.entries=modified,
    rss = rss.sum
    #rss.log=q.min.rss.log, 
    #n.cyc=cyc
  )
  return(output)
}
