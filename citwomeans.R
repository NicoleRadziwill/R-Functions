ci.twomeans <- function(ybar1,ybar2,n1,n2,sd1=NULL,sd2=NULL,
   sigma1=NULL,sigma2=NULL,eq.var=FALSE,conf.level=0.95,
   vname="[difference between the quantitative variables]") {
   # Example: ci.twomeans(18.27,16.78,20,19,sqrt(8.74),sqrt(6.58),eq.var=FALSE)
      # We assume a conf.level of 0.95 and eq.var=FALSE just so
      # you have to change it to TRUE if you REALLY mean it
      z.star <- NULL
      t.star <- NULL
      ME <- NULL
      cint <- NULL
      my.df <- NULL # we only need this if we DO know the population variance
      diff.ybars <- ybar1-ybar2
         if(!is.null(sigma1)) {
            # we've been given two population SD's so we
            # DO know the population variance
            z.star <- qnorm(conf.level+((1-conf.level)/2))
            ME <- z.star*sigma1*(sqrt((1/n1)+(1/n2)))
         }
         if(!is.null(sd1)) {
            # we've been given two sample SD's so we
            # DON'T know the population variance
            if(eq.var==FALSE) {
                my.df <- (((sd1^2/n1)+(sd2^2/n2))^2)/((((sd1^2/n1)^2)/(n1-1))+(((sd2^2/n2)^2)/(n2-1)))
                t.star <- qt(conf.level+((1-conf.level)/2),df=my.df)
                 ME <- t.star*sqrt((sd1^2/n1)+(sd2^2/n2))
            } else {
                my.df <- n1+n2-1
                s.pooled <- (((n1-1)*(sd1^2))+((n2-1)*(sd2^2)))/my.df
                t.star <- qt(conf.level+((1-conf.level)/2),df=my.df)
                ME <- t.star*s.pooled*sqrt((1/n1)+(1/n2))
            }
         }
      cint <- diff.ybars + c(-ME,ME)
      short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
              (conf.level*100), diff.ybars, ME, cint[1], cint[2])
      verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
      (conf.level*100), vname, cint[1], cint[2])
   return(list(short=short,verbose=verbose,cint=cint))
}
