MandelPvalue <-
function(hfobj)
{
bb <- nlevels(hfobj$tall$block)
tt <- nlevels(hfobj$tall$trt)
ymtx <- matrix(hfobj$tall$y,nrow=bb,ncol=tt,byrow=T)
coldevs <- apply(ymtx,2,mean)-mean(ymtx)
rowdevs <- apply(ymtx,1,mean)-mean(ymtx)
SSBlock <- tt*sum(rowdevs^2)
SSTrt <- bb*sum(coldevs^2)
SSTot <- (bb*tt-1)*var(hfobj$tall$y)
slopes <- ymtx %*% coldevs/sum(coldevs^2)
# SSMandel <- (t(slopes) %*% rowdevs)^2/sum(rowdevs^2) * sum(coldevs^2)
SSMandel <- sum((slopes-1)^2) * sum(coldevs^2)
SSE <- SSTot-SSMandel-SSBlock-SSTrt
dfE <- ((bb-1)*(tt-2))
MSE <- SSE/dfE
Fratio <- (SSMandel/(bb-1))/MSE
pvalue <- 1-pf(Fratio,(bb-1),dfE)
SumSq <- c(SSBlock=SSBlock,SSTrt=SSTrt,SSMandel=SSMandel,SSE=SSE,SSTot=SSTot)
list(pvalue=pvalue,SumSq=SumSq)
#list(mandel.pvalue=pvalue)
}
