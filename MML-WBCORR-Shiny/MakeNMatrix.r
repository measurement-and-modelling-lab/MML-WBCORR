function(NList, hypothesis) {
    rows <- nrow(hypothesis)
    NMatrix <- c()

    for (i in 1:rows) {
        NMatrix[[i]] <- NList[[hypothesis[i,1]]]-1
    }

    NMatrix <- diag(NMatrix)

    return(NMatrix)
}
