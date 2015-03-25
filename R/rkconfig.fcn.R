rkconfig.fcn <-
function(block)
{
bvalues <- as.vector(names(table(block)))
b <- length(table(block))
cc <- 2^(b-1)-1-b
rkconfig.mtx <- matrix(NA,nrow=length(block),ncol=cc)
counter <- 1
#if(is.even(b))
if(1-(b%%2))
{
if(b==4)
{
for(j in 1:3){
combo <- combn(b,2)
rkconfig.mtx[,j] <- 1*is.element(block,combo[,j])
}
}
else
{
for(g in 2:((b/2)-1))
{
ulim <- choose(b,g)
for(j in 1:ulim)
{
# print(c(b,g,j,ulim))
combo <- combn(b,g)
rkconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
g <- g+1
ulim <- choose(b,g)/2
for(j in 1:ulim)
{
combo <- combn(b,g)[,1:ulim]
rkconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
}
#else if (is.odd(b))
else if (b%%2)
{
for(g in 2:((b-1)/2))
{
ulim <- choose(b,g)
for(j in 1:ulim)
{
combo <- combn(b,g)
rkconfig.mtx[,counter] <- 1*is.element(block,combo[,j])
counter <- counter+1
}
}
}
rkconfig.mtx
}
