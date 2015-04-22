ci.mean <- function(n,ybar,s=NULL,sigma=NULL,conf.level=0.95,
   vname="[difference between the quantitative variables]") {
      z.star <- NULL
      t.star <- NULL
      ME <- NULL
      cint <- NULL
         if(!is.null(sigma)) {
	    # we know the population variance so look up z*
            z.star <- qnorm(conf.level+((1-conf.level)/2))
	    ME <- z.star*(sigma/sqrt(n))
         }
         if(!is.null(s)) {
	    # We only know the sample sd so look up t*
            t.star <- qt(conf.level+((1-conf.level)/2),df=(n-1))
	    ME <- t.star*(s/sqrt(n))
         }
      cint <- ybar + c(-ME,ME)
      short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
              (conf.level*100), ybar, ME, cint[1], cint[2])
      verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
	      (conf.level*100), vname, cint[1], cint[2])
   return(list(short=short,verbose=verbose,cint=cint))
}
