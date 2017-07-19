	mode <- function(x) {
		uniq.vals <- unique(x)
		uniq.vals[which.max(tabulate(match(x, uniq.vals)))]
	}
