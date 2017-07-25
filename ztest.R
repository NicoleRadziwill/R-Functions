z.test <- function(x,n,p=NULL,conf.level=0.95,alternative="less") {
   ts.z <- NULL
   cint <- NULL
   p.val <- NULL
   phat <- x/n
   qhat <- 1 - phat
   # If you have p0 from the population or H0, use it.
   # Otherwise, use phat and qhat to find SE.phat:
      if(length(p) > 0) {
         q <- 1-p
         SE.phat <- sqrt((p*q)/n)
         ts.z <- (phat - p)/SE.phat
         p.val <- pnorm(ts.z)
         if(alternative=="two.sided") {
            if(p.val > 0.5) {
               p.val <- 1 - p.val
               p.val <- p.val * 2
            }
         }
         if(alternative=="greater") {
            p.val <- 1 - p.val
         }
      } else {
      # If all you have is your sample, use phat to find
      # SE.phat, and don't run the hypothesis test:
         SE.phat <- sqrt((phat*qhat)/n)
      }
      cint <- phat + c(
         -1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat),
         ((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat) )
   return(list(estimate=phat,ts.z=ts.z,p.val=p.val,cint=cint))
}
