function(data) {
    
    A <- length(data)

    skew_table <- list()
    kurt_table <- list()


    for (i in 1:A) {

        X <- data[[i]]

        n <- nrow(X)
        p <- ncol(X)


        S <- cov.wt(X, method="ML")
        S <- S[[1]]

        difT <- c()
        for (j in 1:p) {
            difT <- c(difT, as.vector(X[,j] - mean(X[,j])))
        }
        difT <- matrix(difT, nrow=n, ncol=p)

        D <- difT%*%solve(S)%*%t(difT)
        b1p <- sum(colSums(D^3))/n^2
        b2p <- sum(diag(D^2))/n


        df = (p*(p+1)*(p+2))/6
        MST = (n*b1p)/6
        P1 = 1 - pchisq(MST,df)

        MKT = (b2p-(p*(p+2)*(n-1)/(n+1)))/(sqrt((8*p*(p+2))/n))
        P2 = 2*(1-pnorm(abs(MKT), 0, 1))

        ## Assemble row of table
        skew_table[[i]] <- c(i, b1p, MST, df, P1)
        kurt_table[[i]] <- c(i, b2p, MKT, P2)

    }

    skew_table <- do.call(rbind, skew_table)
    colnames(skew_table) <- c("Group", "Multivariate Skewness", "Chi Square", "df", "plevel")

    kurt_table <- do.call(rbind, kurt_table)
    colnames(kurt_table) <- c("Group", "Multivariate Kurtosis", "Z statistic", "plevel (2-tailed)")

    return(list(skew_table, kurt_table))
}
