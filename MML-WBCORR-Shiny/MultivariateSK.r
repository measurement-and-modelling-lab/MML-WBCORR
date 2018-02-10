function(data) {
  
    A <- length(data)

    skew_table <- c("Group", "Multivariate Skewness", "Chi Square", "df", "plevel")
    kurt_table <- c("Group", "Multivariate Kurtosis", "Z statistic", "plevel (2-tailed)")

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

        skew_table <- rbind(skew_table, round(c(A, b1p, MST, df, P1),3))

        if (skew_table[i+1,5] == 0) {
                skew_table[i+1,5] <- '< 0.001'
        }

        kurt_table <- rbind(kurt_table, round(c(A, b2p, MKT, P2),3))

        if (kurt_table[i+1,4] == 0) {
                kurt_table[i+1,4] <- '< 0.001'
        }


    }

    return(list(skew_table, kurt_table))
}
