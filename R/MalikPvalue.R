MalikPvalue <- function(hfobj,N=1000){
  tallset<-hfobj$tall
  
  y<-tallset$y
  block<-tallset$block
  trt<-tallset$trt
  
  b<-max(as.numeric(as.character(block)))
  t<-max(as.numeric(as.character(trt)))
  
  mod<-lm(y~block + trt)
  r<-resid(mod)
  names(r)<-NULL
  
  rmat<-matrix(r,nrow=length(r),ncol=1)
  kmean<-kmeans(x=rmat,centers=3,nstart=100)
  assn<-kmean$cluster
  modclus<-lm(y~block + trt +as.factor(assn))
  amodclus<-anova(modclus)
  Tc<- (amodclus[3,2]/amodclus[3,1])/(amodclus[4,2]/amodclus[4,1]) 
  
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
    Tcsim[i]<- (amodclussim[3,2]/amodclussim[3,1])/(amodclussim[4,2]/amodclussim[4,1]) #I think this might be right but double check when ILL gets back to me
  }
  
  malik.p<-mean(Tc>Tcsim)
  cat(paste("(pvalue from Malik's test estimated with N=",N,"Monte Carlo datasets) \n"))
  list(pvalue=malik.p)
}
