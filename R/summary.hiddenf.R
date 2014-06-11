summary.hiddenf <-
function(object,...){
  b<-max(as.numeric(object$tall$block))
  grpassn<-object$config.vector[object$tall$trt==1]
  print(anova(object))
  cat('Number of configurations:',length(object$pvalues),'\n')
  cat('Hidden additivity p-value (Bon):',round(object$adjpvalue,4),'\n')
  cat('Blocks in group 1:',c(1:b)[grpassn==1], '\n')
  cat('Blocks in group 2:',c(1:b)[grpassn==0], '\n')
  output <- list(group1=c(1:b)[grpassn==1],group2=c(1:b)[grpassn==0])
  class(output) <- "summary.hiddenf"
  output
}
