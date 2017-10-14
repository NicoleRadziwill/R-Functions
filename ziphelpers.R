# ziphelpers.R
# from http://www.statistik.lmu.de/institut/ag//leisch/teaching/fcim11/fcim-beispiele-04-ziphelpers.R


class2prob <- function(x) {
  x <- as.factor(x)
  p <- matrix(0, nrow = length(x), ncol = nlevels(x))
  p[cbind(seq_len(nrow(p)), as.integer(x))] <- 1
  p
}

scale <- function(x) {
    rx <- range(x)
    rt <- c(0, 2)

    scale <- (rx[2] - rx[1]) / (rt[2] - rt[1])
    x2 <- rt[1] + ((x - rx[1]) / scale)

    (x2 - 1)
}


as.zipimage <- function(x) {
    raster <- x$bitmap
    raster16 <- matrix(NA, 16, 16)

    for ( i in 1:16 ) {
        ii <- seq((((i - 1) * 10) + 1), (i * 10))
        for ( j in 1:16 ) {
            jj <- seq((((j - 1) * 10) + 1), (j * 10))
            raster16[i, j] <- mean(raster[ii, jj])
        }
    }

    raster16 <- scale(raster16)

    structure(list(vector=x$vector,
                   bitmap=structure(raster16, class='bhwdigit')),
              class='hwdigit')
}


image2zip <- function(x) {
    matrix(as.numeric(x), nrow=1)
}


find.digits <- function(zip, n, digits=0:9) {
    structure(lapply(digits,
                     function(i)
                     sample(which(zip[,1] == i), n)),
              names=digits)
}


plot.digits <- function(zip, n, ...) {
    rows <- find.digits(zip, n)
    digits <- lapply(rows,
                     function(row)
                     do.call(cbind,
                             lapply(row,
                                    function(r) zip2image(zip, r))))
    im <- do.call(rbind, digits)

    image(im, col=gray(256:0/256), zlim=c(0,1), xlab='', ylab='')
}
