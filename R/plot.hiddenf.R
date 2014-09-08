plot.hiddenf <-
function(x,main="Hidden Additivity plot",tfactor="treatment factor",bfactor="blocking factor",legendx="a",...)
{
hfobj <- x
ymtx <- makemtx.fcn(hfobj$tall)
bb <- nrow(ymtx)
tt <- ncol(ymtx)
grpvector <- 1+hfobj$config.vector[1+tt*(0:(bb-1))]
matplot(t(ymtx),type="l",col=grpvector,main=main,xaxt="n",ylab="y",xlab=tfactor,lty=grpvector,...) # xlab=tfactor
# xlim=c(1,tt+1))
#legend(tt,max(ymtx),1:5,lty=1:5,col=grpvector)
#legend("topright",1:5,lty=1:5,col=grpvector,title=paste(bfactor))
if (legendx > "a"){
legend(x=legendx,legend=1:bb,lty=1:bb,col=grpvector,title=bfactor)}
axis(1,at=1:tt,labels=names(table(hfobj$tall$trt)),cex.axis=1.2)
}
