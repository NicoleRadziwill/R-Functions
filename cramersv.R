cramers.v <- function(xsq) {
		a <- dim(xsq$observed)[1]  # number of rows
		b <- dim(xsq $observed)[2] # number of columns
		t <- min(a,b)-1
		sqrt(xsq$statistic/(sum(xsq$observed)*t))
}
