# These functions were written by Manuel Eugner and Bettina Grün at Ludwig-Maximilians-Universität München.

plot.canvas <- function() {
    plot(0:16, 0:16, type="n", xaxs="i",
         yaxs="i", xlab="", ylab="")
    grid(16, 16)
}


drawing.canvas <- function() {

    mousedown.event <- function(buttons, x, y) {
        if ( x > 0.85 && y > 0.85 )
            return("Done")
        NULL
    }

    mouseup.event <- function(buttons, x, y) {
        released <<- TRUE
        segments <<- c(segments, n)
        NULL
    }

    mousemove.event <- function(buttons, x, y) {
        if ( length(buttons) > 0 ) {
            n <<- n + 1

            plx <- grconvertX(x, "ndc", "user")
            ply <- grconvertY(y, "ndc", "user")

            points <<- rbind(points, c(plx, ply))

            if ( !released )
                ix <- c(n, n-1)
            else
                ix <- n

            lines(points[ix, 1],
                  points[ix, 2], lwd=50)


            plxr <- round(plx * 10, 0)
            plyr <- round(ply * 10, 0)

            raster[as.matrix(expand.grid(seq(plxr - raster.radius,
                                         plxr + raster.radius),
                                         seq(plyr - raster.radius,
                                         plyr + raster.radius)))] <<- 1

            released <<- FALSE
        }

        NULL
    }

    released <- TRUE
    points <- matrix(NA, ncol=2, nrow=0)
    raster <- matrix(0, ncol=160, nrow=160)
    raster.radius <- 7
    n <- 0
    segments <- NULL

    plot.canvas()
    getGraphicsEvent("Zum Beenden in die rechte obere Ecke klicken!",
                     onMouseDown=mousedown.event,
                     onMouseMove=mousemove.event,
                     onMouseUp=mouseup.event)


    structure(list(vector=structure(points, segments=segments, class="vhwdigit"),
                   bitmap=structure(raster, class="bhwdigit")),
              class="hwdigit")
}


plot.vhwdigit <- function(x, ...) {
    plot.canvas()

    segments <- unique(c(0, attr(x, "segments"), nrow(x)))

    for ( i in 1:(length(segments) - 1) )
        lines(x[seq(segments[i]+1, segments[i+1]),], lwd=50)
}


plot.bhwdigit <- function(x, ...) {
    n <- nrow(x)
    image(x=0:n, , y=0:n, z=x,
          xlim=c(0, n), ylim=c(0, n), zlim=c(0, 1),
          xaxs="i", yaxs="i", xlab="", ylab="",
          col=gray(256:0/256))
    grid(n, n)
    box()
}


plot.hwdigit <- function(x, ...) {
    par(mfrow=c(1, 2))
    plot(x$vector, ...)
    plot(x$bitmap, ...)
}


hwcanvas <- function(full=FALSE) {
    x <- as.zipimage(drawing.canvas())

    if ( full )
        x
    else
        x$bitmap
}
