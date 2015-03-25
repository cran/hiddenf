plot.hiddenf <-
function(x,y=NULL,main="Hidden Additivity Plot",tfactor="Treatment Factor",bfactor="Blocking Factor",colorvec=c("black","red"),legendx=FALSE,center=FALSE,...)
{
hfobj <- x
ymtx <- makemtx.fcn(hfobj$tall)
bb <- nrow(ymtx)
tt <- ncol(ymtx)
grpvector <- 1+hfobj$config.vector[1+tt*(0:(bb-1))]
#matplot(t(ymtx),type="l",col=grpvector,main=main,xaxt="n",ylab="y",xlab=tfactor,lty=grpvector,...) # xlab=tfactor
if(center==TRUE)
{
ymtx <- ymtx-apply(ymtx,1,mean)
}
matplot(t(ymtx),type="l",col=colorvec[grpvector],main=main,xaxt="n",ylab="y",xlab=tfactor,lty=1:bb,...) # xlab=tfactor
# xlim=c(1,tt+1))
#legend(tt,max(ymtx),1:5,lty=1:5,col=grpvector)
#legend("topright",1:5,lty=1:5,col=grpvector,title=paste(bfactor))
if (legendx == TRUE){
legend(x=locator(1),legend=1:bb,lty=1:bb,col=colorvec[grpvector],title=bfactor)}
axis(1,at=1:tt,labels=names(table(hfobj$tall$trt)),cex.axis=1.2)
}
