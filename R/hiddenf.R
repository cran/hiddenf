hiddenf <-
function(ymtx)
{
obj.tall <- maketall.fcn(ymtx)
block <- obj.tall$block
trt <- obj.tall$trt
y <- obj.tall$y
# not sure if this works yet, June 13, 2013
date1<- date()
# takes as input a data frame with components y,block,trt
rconfig.mtx <- rconfig.fcn(block)
b <- length(table(block))
trt <- as.factor(trt)
cc <- 2^(b-1)-1
pvalues <- rep(NA,cc)
for(i in 1:cc)
{
# y.tmpout <- lm(y ~ as.factor(rconfig.mtx[,i])*trt + block/rconfig.mtx[,i])
y.tmpout <- lm(y ~ as.factor(rconfig.mtx[,i])*trt + block/as.factor(rconfig.mtx[,i]))
pvalues[i] <- anova(y.tmpout)$P[4]
}
pvalue <- cc*min(pvalues)
pvalue <- min(1,pvalue)
config <- which.min(pvalues)
config.vector <- rconfig.mtx[,config]
tall <- list(y=y,block=block,trt=trt)
date2<- date()
hfout <- list(pvalues=pvalues,adjpvalue=pvalue,config=config,config.vector=config.vector,tall=tall,cc=cc,date1=date1,date2=date2)
# added adjpvalue, March 10, 2014
class(hfout) <- "hiddenf"
# takes as input a data frame with components y,block,trt
return(hfout)
}
