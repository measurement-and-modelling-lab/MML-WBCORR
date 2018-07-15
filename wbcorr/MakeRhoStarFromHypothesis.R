function(hypothesis) {
    rows <- nrow(hypothesis)
    X <- matrix(0, nrow = rows, ncol = 1)

    for (i in 1:rows) {
        if (hypothesis[[i,4]] == 0) {
            X[[i,1]] <- hypothesis[[i,5]]
        }
    }

    return(X)
}
