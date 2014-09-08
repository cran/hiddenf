makemtx.fcn <-
function(tall)
{
tapply(tall$y,list(tall$block,tall$trt),identity)
}
