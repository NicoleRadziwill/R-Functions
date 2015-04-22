ci.paired <- function(n,dbar,s.d,conf.level=0.95,
   vname="[difference between the quantitative variables]") {
   # Example: ci.paired(8,3.625,2.066,vname="difference between number of recalled words")
      t.star <- NULL
      ME <- NULL
      cint <- NULL
         if(!is.null(s.d)) {
            t.star <- qt(conf.level+((1-conf.level)/2),df=(n-1))
	    ME <- t.star*(s.d/sqrt(n))
         }
      cint <- dbar + c(-ME,ME)
      short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
              (conf.level*100), dbar, ME, cint[1], cint[2])
      verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
	      (conf.level*100), vname, cint[1], cint[2])
   return(list(short=short,verbose=verbose,cint=cint))
}
