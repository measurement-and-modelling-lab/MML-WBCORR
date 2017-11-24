function(hypothesis) {
    rows <- nrow(hypothesis)

    parlist <- unique(hypothesis[, 4])
    parlist <- parlist[parlist != 0]
    numberOfParameters <- length(parlist)

    if ( numberOfParameters == 0) {
        return()
    }

    D <- matrix(0, rows, numberOfParameters)

    for (i in 1:rows) {
        p <- hypothesis[i,4]
        D[i,p] <- 1
    }

    return(D)
}
