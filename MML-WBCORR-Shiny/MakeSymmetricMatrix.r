function(R) {

    p <- ncol(R)
    RR <- matrix(0, nrow = p, ncol = p)

    for (i in 1:p) {
        for (j in 1:i) {
            RR[i,j] <- R[i,j]
            RR[j,i] <- R[i,j]
        }
    }
return(RR)
}
