function(NList, hypothesis) {
    rows <- nrow(hypothesis)
    NMatrix <- c()

    for (i in 1:rows) {
        NMatrix[[i]] <- NList[[hypothesis[i,1]]]-1
    }

    if (rows > 1) { # if hypothesis pertains to only 1 correlation then this should simply = N, but if diag() is only given a single number then it creates an NxN matrix of 0s with 1s along the diagonal
      NMatrix <- diag(NMatrix)
    }

    return(NMatrix)
}
