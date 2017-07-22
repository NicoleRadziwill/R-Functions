# John Gosink's Intervals Plotter (from http://gosink.org/?page_id=120)
plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor='red', ...) {
	xOrder  <- order(x)
	x       <- x[xOrder]  
	y       <- y[xOrder]
        fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
	newX    <- data.frame(x=jitter(x))
	fitPred <- predict.lm(fit, newdata=newX, interval=interval, 			level=level, ...)
	abline(lm(y ~ x), col=regressionColor)
	lines(newX$x, fitPred[,2], lty=2, ...)
	lines(newX$x, fitPred[,3], lty=2, ...)
}
