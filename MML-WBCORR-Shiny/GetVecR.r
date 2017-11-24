function(RList, hypothesis) {

    rows <- nrow(hypothesis)
    vecr <- rep(0, rows)

    for (i in 1:rows) {
        vecr[[i]] <- RList[[hypothesis[i,1]]][[hypothesis[i,2],hypothesis[i,3]]]
    }

    return(vecr)
}
