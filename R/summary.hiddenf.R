summary.hiddenf <-
function(object,...){
  b<-max(as.numeric(names(table(object$tall$block))))
  t<-max(as.numeric(names(table(object$tall$trt))))
  ymtx <- matrix(object$tall$y,byrow=T,nrow=b,ncol=t)
  grpassn<-object$config.vector[object$tall$trt==1]
#  print(anova(object))
  cat('Number of configurations:',object$cc,'\n')
#  cat('First several pvalues testing for nonadditivity \n  after Bonferroni adjustment:',round(object$pvalue[1:6],4),'... \n\n')
grp1rows <- c(1:b)[grpassn==1]
grp2rows <- c(1:b)[grpassn==0]
grp1means <- apply(ymtx[grp1rows,],2,mean)
grp2means <- apply(ymtx[grp2rows,],2,mean)
  cat('Minimum adjusted pvalue:',object$adjpvalue,'\n\n')
  cat('Blocks in group 1:',grp1rows, '\n')
  cat('Blocks in group 2:',grp2rows, '\n\n')
  cat('Column means for grp 1:',grp1means,'\n')
  cat('Column means for grp 2:',grp2means,'\n\n')
  output <- list(group1=grp1rows,group2=grp2rows,grp1means=grp1means,grp2means=grp2means)
  class(output) <- "summary.hiddenf"
  return(output)
}
