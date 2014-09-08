print.hiddenf <-
function(x,...)
{
cat("The F-Test for Hidden Additivity \n")
hfanova <- anova(x)
Fratio <- prettyNum(hfanova$F[3],digits=4)
pvalue <- prettyNum(x$adjpvalue,digits=4)
cat(paste("F=",Fratio," p-value =",pvalue," df=",hfanova$Df[4],",",hfanova$Df[5],"\n",sep=""))
cat(paste("(Bonferroni-adjusted for all",x$cc,"possible configurations) \n"))
output <- list(Fratio=Fratio,adjpvalue=pvalue)
class(output) <- "print.hiddenf"
return(output)
}
