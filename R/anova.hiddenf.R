anova.hiddenf <-
function(object,...)
{
y <- object$tall$y
block <- object$tall$block
trt <- object$tall$trt
grp <- as.factor(object$config.vector)
lm.out <- lm(y~grp*trt+block/grp)
anova.out <- anova(lm.out)
return(anova.out)
}
