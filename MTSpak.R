library(MASS)            # for ginv function
library(qualityTools)    # for taguchiDesign function
library(tidyverse)       # for data handling, pipes, and plotting

# Functions to compute Mahalanobis Distances (MDs) for "good" and "bad" groups
# Takes two data frames as inputs

computeMDs <- function(good, bad) {
   if (!is.data.frame(good) || !is.data.frame(bad)) { 
      stop("At least one input is not a data frame")
   } else {
      # Compute means and sd's for "good" group
      xbars <- colMeans(good)
      sds <- apply(good, 2, sd)
      pinv <- 1/ncol(good)
      Rinv <- ginv(cor(good))

      # Compute MDs for bad group by using means and sds of "good" group as a baseline
      z0s <- apply(bad, 1, function(x) (x-xbars)/sds)  
      MD.bad <- pinv * diag( (t(z0s) %*% Rinv %*% z0s) )
   
      # Compute MDs for good group
      z0s.good <- apply(good, 1, function(x) (x-xbars)/sds)
      MD.good <- pinv * diag( (t(z0s.good) %*% Rinv %*% z0s.good) )
   
   return(list(MD.bad=MD.bad, MD.good=MD.good))
   }
}
                        
generateTDO <- function(good) {
      # This function can take as input either the good or bad data frame, since both have same ncol.
      # Alternatively, you can just give it the number of IVs directly and it will still work.
      if (!is.data.frame(good)) { 
          ivs <- good
      } else {
          ivs <- ncol(good)
      }
      if (ivs %in% c(2:3)) {
          tdo <- taguchiDesign("L4_2")@design[,1:ivs] %>% replace(.=="2",0)
      } else if (ivs %in% c(4:7)) {
          tdo <- taguchiDesign("L8_2")@design[,1:ivs] %>% replace(.=="2",0)
      } else if (ivs %in% c(8:11)) { 
          tdo <- taguchiDesign("L12_2")@design[,1:ivs] %>% replace(.=="2",0)
      } else if (ivs %in% c(12:15)) {
          tdo <- taguchiDesign("L16_2")@design[,1:ivs] %>% replace(.=="2",0)
      } else if (ivs==1 || ivs > 15) {
          stop(paste0("Number of independent variables ",ivs," is not supported"))
      }
   return(tdo=tdo)
}

# Functions to compute Taguchi Signal to Noise (SN)
ltb <- function(x) { 
    -10*log10( (1/(length(x))) * sum(1/(x^2) )) 
}   # Larger-the-better

stb <- function(x) { 
    -10*log10( (1/(length(x))) * sum(  (x^2) )) 
}   # Smaller-the-better

dyn.sn <- function(x, y) { 
   stb(x) - ltb(y) 
}
