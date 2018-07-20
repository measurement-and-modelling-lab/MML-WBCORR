function (a,i,j,b,k,h,R,moments) {
    ## Called by ComputeWBCorrChiSquare.R
    ## a, i and j are the group, row and column number of one correlation from the hypothesis matrix
    ## b, k and h are the group, row and column number of another (possibly the same) correlation from the hypothesis matrix
    ## R is the list of correlation matrices if using ADF, and the list of OLS matrices if using TSADF
    ## moments are the fourth order moments, i.e. values on kurtosis
    ## output is the covariance of the two correlations

    FRHO <- dget("./wbcorr/FRHO.R")

    if (a != b) {
        Cov <- 0
    } else {
        term1 <- FRHO(i,j,k,h,moments[[a]])
        term2 <- 1/4*R[[a]][[i,j]]*R[[a]][[k,h]]*(FRHO(i,i,k,k,moments[[a]]) + FRHO(j,j,k,k,moments[[a]]) + FRHO(i,i,h,h,moments[[a]]) + FRHO(j,j,h,h,moments[[a]]))
        term3 <- 1/2*R[[a]][[i,j]]*(FRHO(i,i,k,h,moments[[a]]) + FRHO(j,j,k,h,moments[[a]]))
        term4 <- 1/2*R[[a]][[k,h]]*(FRHO(i,j,k,k,moments[[a]]) + FRHO(i,j,h,h,moments[[a]]))
        Cov <- term1 + term2 - term3 - term4
    }
    return(Cov)
}
