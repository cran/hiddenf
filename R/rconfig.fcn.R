rconfig.fcn <-
function(block)
{
bvalues <- as.vector(names(table(block)))
b <- length(table(block))
if(b > 15){stop("package not yet ready for b>15")}
cc <- 2^(b-1)-1
rconfig.mtx <- matrix(NA,nrow=length(block),ncol=cc)
counter <- 1
#if(is.even(b))
if((b %% 2)==0)
{
for(g in 1:((b/2)-1))
{
ulim <- choose(b,g)
for(j in 1:ulim)
{
combo <- combn(b,g)
rconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
g <- g+1
print(g)
ulim <- choose(b,g)/2
for(j in 1:ulim)
{
combo <- combn(b,g)[,1:ulim]
rconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
#else if (is.odd(b))
else if((b %% 2)==1)
{
for(g in 1:((b-1)/2))
{
ulim <- choose(b,g)
for(j in 1:ulim)
{
combo <- combn(b,g)
rconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
}
rconfig.mtx
}
