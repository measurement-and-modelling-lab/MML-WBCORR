function(RList, hypothesis) {
    ## Called by ComputeWBCorrChiSquare.R
    ## RList is the list of correlation matrices
    ## Hypothesis is the hypothesis matrix
    ## Returns a vector of the correlations referenced in the hypothesis matrix

    rows <- nrow(hypothesis)
    vecr <- rep(0, rows)

    for (i in 1:rows) {
        vecr[i] <- RList[[hypothesis[i,1]]][hypothesis[i,2],hypothesis[i,3]]
    }

    return(vecr)
}
