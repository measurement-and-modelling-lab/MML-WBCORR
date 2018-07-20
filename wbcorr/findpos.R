function(i, j, k, h) {
    ## Called by FRHO.R
    ## i and j are the row and column number of one correlation from the hypothesis matrix
    ## k and h are the row and column number of another (possibly the same) correlation from the hypothesis matrix
    ## Returns the index number of the kurtosis for the two correlations in the moments list

    ordered.indices <- sort(c(i, j, k, h), decreasing = TRUE)
    a <- ordered.indices[1]
    b <- ordered.indices[2]
    c <- ordered.indices[3]
    d <- ordered.indices[4]

    post <- (a-1)*a*(a+1)*(a+2)/24 + (b-1)*b*(b+1)/6 + c*(c-1)/2 + d

    return(post)
}
