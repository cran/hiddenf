MalikTab <-
function(t,b,N=1000){
  block=as.factor(rep(1:b,each=t))
  trt<-as.factor(rep(seq(1:t),b))
  Tcsim<-c()
  for(i in 1:N){
    ysim<-rnorm((t*b),0,1)
    modsim<-lm(ysim~block + trt)
    rsim<-resid(modsim)
    rmatsim<-matrix(rsim,nrow=length(rsim),ncol=1)
    kmeansim<-kmeans(x=rmatsim,centers=3,nstart=100)
    assnsim<-kmeansim$cluster
    modclussim<-lm(ysim~block + trt +as.factor(assnsim))
    amodclussim<-anova(modclussim)
    Tcsim[i]<- (amodclussim[3,2]/amodclussim[3,1])/(amodclussim[4,2]/amodclussim[4,1])}
    return(list(Tcsim=sort(Tcsim),q=c(r=t,c=b,quantile(Tcsim,c(.99,.95,.9)))))
  }
