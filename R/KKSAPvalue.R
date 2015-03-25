KKSAPvalue <-
function(hfobj)
{
bb <- nlevels(hfobj$tall$block)
if(bb<4){print("KKSA needs at least 4 levels of blocking factor");return(list(pvalue=NA))}
block <- hfobj$tall$block
trt <- hfobj$tall$trt
y <- hfobj$tall$y
rkconfig.mtx <- rkconfig.fcn(block)
block <- as.factor(block)
b <- length(table(block))
if(b<4)stop("KKSA only applicable if b > 3")
trt <- as.factor(trt)
tt <- length(table(trt))
cc <- 2^(b-1)-1-b # no singletons
fvalues <- rep(NA,cc)
pvalues <- rep(NA,cc)
for(i in 1:cc)
{
i1 <- (rkconfig.mtx[,i]==1)
i2 <- !i1
y0.tmpout <- lm(y[i1] ~ trt[i1] + block[i1])
ms0 <- anova(y0.tmpout)[3,3]
df0 <- (sum(i1)/tt-1)*(tt-1)
y1.tmpout <- lm(y[i2] ~ trt[i2] + block[i2])
ms1 <- anova(y1.tmpout)[3,3]
df1 <- (sum(i2)/tt-1)*(tt-1)
fstat <- ms0/ms1
finv <- ms1/ms0
if(fstat>finv){
fmax <- fstat
dfn <- df0
dfd <- df1
}
else{
fmax <- finv
dfn <- df1
dfd <- df0
}
# print(c(i,fstat,finv,fmax))
fvalues[i] <- fmax
#pvalues[i] <- 1-pf(fmax,dfn,dfd)
pvalues[i] <- min(1,2*(1-pf(fmax,dfn,dfd)))
}
pvalue <- min(pvalues)*cc
pvalue <- min(1,pvalue)
config <- which.min(pvalues)
config.vector <- rkconfig.mtx[,config]
grp.vector <- 1+config.vector[1+tt*(0:(b-1))]
date2<- date()
tall <- list(y=y,block=block,trt=trt)
# kksa.out <- list(pvalues=pvalues,pvalue=pvalue,tall=tall,fvalues=fvalues)
#kksa.out <- list(pvalues=pvalues,pvalue=pvalue,config=config,config.vector=config.vector,tall=tall)
KKSA.out <- list(pvalue=pvalue,grp.vector=grp.vector,tall=tall)
return(KKSA.out)
}
