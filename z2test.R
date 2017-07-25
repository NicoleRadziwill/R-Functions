z2.test <- function(x1,n1,x2,n2,conf.level=0.95,
   alternative="two.sided") {
   ts.z <- NULL
   cint <- NULL
   p.val <- NULL
   phat1 <- x1/n1
   qhat1 <- 1 - phat1
   phat2 <- x2/n2
   qhat2 <- 1 - phat1
   diff.phats <- phat1 - phat2
   pooled.p <- (x1 + x2)/(n1 + n2)
   pooled.q <- 1 - pooled.p
   SE.diffs <- sqrt( ((phat1*qhat1)/n1) + ((phat2*qhat2)/n2) )
   SE.pooled <- sqrt(pooled.p*pooled.q*((1/n1)+(1/n2)))
   # Why two SE's? SE.pooled is used in the calculation of
   # the test statistic z. We can pool because we are making
   # the assumption in the null hypothesis that there is no
   # difference between the two proportions.
   ts.z <- diff.phats/SE.pooled
   p.val <- pnorm(ts.z) # defaults to alternative="less"
   if(alternative=="two.sided") {
      p.val <- p.val * 2
          if(p.val > 0.5) {
             p.val <- p.val - 0.5  
          }
      if(p.val > 1) { p.val = 1 }
   }
   if(alternative=="greater") {
      p.val <- 1 - p.val
   }
   cint <- diff.phats + c(
      -1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.diffs),
      ((qnorm(((1 - conf.level)/2) + conf.level))*SE.diffs) )
   return(list(estimate=diff.phats,ts.z=ts.z,p.val=p.val,cint=cint))
}
