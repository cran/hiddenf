hiddenf <-
function(ymtx){
  date1=date()
  a <- ncol(ymtx)
  b <- nrow(ymtx)
  if(b > 20){cat("Due to computation time, \n hiddenf is unvailable for b > 20 \n");stop}
  else{
  tall <- maketall.fcn(ymtx)
  cc <- 2^(b-1)-1
  ra <- t(t(ymtx - apply(ymtx,1,mean) + mean(ymtx))-apply(ymtx,2,mean))
  PSS <- (1/(1-1/b))*apply(ra^2,1,sum)
  ind <- which.max(PSS)
  combmax <- ind
  PSSmax <- PSS[ind]
  if(b > 3){
    if(b %% 2 ==1){
      niter = round((b-1)/2)
      for(j in 2:niter){
        combs <- combn(b,j)
        PSS <- (1/(j*(1-j/b)))*apply(combs, 2, computePSS, ra)
        ind <- which.max(PSS)
        if(PSSmax < PSS[ind]){
          combmax <- combs[,ind]
          PSSmax <- PSS[ind]
        }
      }
    }
    else{
      niter = round(b/2 - 1)
      if(niter > 1){
        for(j in 2:niter){
          combs <- combn(b,j)
          PSS <- (1/(j*(1-j/b)))*apply(combs, 2, computePSS, ra)
          ind <- which.max(PSS)
          if(PSSmax < PSS[ind]){
            combmax <- combs[,ind]
            PSSmax <- PSS[ind]
          }
        }
      }
      niter <- niter + 1
      combs <- combn(b,niter)
      ncomb <- round(ncol(combs)/2)
      PSS <- (1/(niter*(1-niter/b)))*apply(combs[,1:ncomb], 2, computePSS, ra)
      ind <- which.max(PSS)
      if(PSSmax < PSS[ind]){
        combmax <- combs[,ind]
        PSSmax <- PSS[ind]
      }
    }
  }
  varY = (sum(ra^2) - PSSmax)/((a-1)*(b-2))
  config.vec <- 1*is.element(tall$block,combmax)
  y.tmpout <- lm(tall$y~config.vec*tall$trt + tall$block/config.vec)
  pvalue <- anova(y.tmpout)$P[4]
  adjpvalue <- cc*pvalue
  adjpvalue <- min(1,adjpvalue)
  date2=date()
  #return(list(maxcomb = combmax, maxPSS = PSSmax, varY = varY))
  hfout <- list(adjpvalue=adjpvalue,config.vector=config.vec,tall=tall,cc=cc,date1=date1,date2=date2)
  class(hfout) <- "hiddenf"
  return(hfout)
}
}
