maketall.fcn <-
function(ymtx)
{
bt <- prod(dim(ymtx))
y <- rep(NA,bt)
block <- rep(NA,bt)
trt <- rep(NA,bt)
bb <- nrow(ymtx)
tt <- ncol(ymtx)
counter <- 0
for(i in 1:bb)
for(j in 1:tt)
{
counter <- counter+1
block[counter] <- i
trt[counter] <- j
y[counter] <- ymtx[i,j]
}
list(y=y,block=as.factor(block),trt=as.factor(trt))
}
