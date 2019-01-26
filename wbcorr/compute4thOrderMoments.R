compute4thOrderMoments <-function( data, jj ) {

    X <- data[[jj]]

    n <- nrow(X)
    p <- ncol(X)
    means <- apply(X, 2, mean)
    sds <- apply(X, 2, sd)
    Z <- sweep(X, 2, means, "-")
    Z <- sweep(Z, 2, sds, "/")

    q <- p*(p+1)*(p+2)*(p+3)/24
    moments <- rep(0, q)

    moments <- .C("compute4thOrderMoments",
                moments = moments,
                Z = Z,
                p = as.integer(p),
                n = as.integer(n))$moments

    moments / (n - 1)

}
