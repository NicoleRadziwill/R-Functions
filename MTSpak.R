library(MASS)            # for ginv function
library(qualityTools)    # for taguchiDesign function
library(tidyverse)       # for data handling, pipes, and plotting

# Functions to compute Mahalanobis Distances (MDs) for "good" and "bad" groups
# Takes two data frames as inputs

computeMDs <- function(good, bad) {
   if (!is.data.frame(good) || !is.data.frame(diagnosis_MT)) { 
      print("Inputs are Bad") 
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
