function(r) {
	zscore <- 0.5 * (log(1+r) - log(1-r))
	rounded <- round(zscore, 4)
	return(rounded)
}
