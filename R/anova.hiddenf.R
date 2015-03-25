anova.hiddenf <-
function(object,...)
{
y <- object$tall$y
block <- object$tall$block
trt <- object$tall$trt
grp <- as.factor(object$config.vector)
lm.out <- lm(y~grp*trt+block/grp)
cat("Pvalues in ANOVA table are NOT corrected for multiplicity \n")
return(anova(lm.out))
}
