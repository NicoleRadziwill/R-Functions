chisq.var <- function(n,sample.var,target.var,alternative="greater",conf.level=0.95, 
   vname="[population variance]") {
   cint <- NULL
   df <- n-1
   ci.tails <- 1-conf.level
      ts.chisq <- (df*sample.var)/target.var
      chisq.upper <- qchisq((conf.level+(ci.tails/2)),df=df)
      chisq.lower <- qchisq((ci.tails/2),df=df)
      area.upper <- 1-pchisq(ts.chisq,df=df)
      area.lower <- pchisq(ts.chisq,df=df)
      if ((ts.chisq < chisq.lower) || (ts.chisq > chisq.upper)) {
          area.both <- paste("Less Than",ci.tails)
      } else {
          area.both <- paste("Greater Than",ci.tails)
      }
      cint <- c((df*sample.var)/chisq.upper,(df*sample.var)/chisq.lower)
      verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
	      (conf.level*100), vname, cint[1], cint[2])
      p.value <- switch(alternative,
	"greater" = area.upper,
	"less" = area.lower,
	"two.sided" = area.both)
      return(list(chisq.upper=chisq.upper,chisq.lower=chisq.lower,ts=ts.chisq,cint=cint,p.value=p.value,verbose=verbose))
}
