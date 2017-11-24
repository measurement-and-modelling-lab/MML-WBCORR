function (data) {

    zr_table <- c('Group','Variable','Missing values','&nbsp;&nbsp;&nbsp;&nbsp;Z<sub>R</sub>&nbsp;&nbsp;&nbsp;&nbsp;', 'plevel (two-tail)')

    m2_table <- c('Group','&nbsp;&nbsp;&nbsp;&nbsp;M<sub>2</sub>&nbsp;&nbsp;&nbsp;&nbsp;','plevel (two-tail)')

    for (jj in 1:length(data)) {

        group <- data[[jj]]
        colnames(group) <- 1:p
        n <- nrow(group)
        p <- ncol(group)

        # isolate the incomplete and complete columns
        incomplete <- group[,colSums(is.na(group)) > 0, drop=FALSE]
        complete <- group[,colSums(is.na(group)) == 0, drop=FALSE]

        while (length(incomplete)*length(complete) > 0) {

            pair <- cbind(incomplete[,1], complete[,1])
            variable <- colnames(incomplete)[[1]]
            incomplete <- incomplete[,-1]

            # subtract from each score the mean of its column
            m <- colMeans(pair, na.rm=TRUE)
            m_matrix <- matrix(rep(m, each=n), nrow=n, ncol=2)
            dev_matrix <- abs(pair - m_matrix)

            # rank the incomplete scores by the value of the corresponding complete scores, then sum the ranks of the NA values
            dev_matrix <- cbind(dev_matrix, rank(dev_matrix[,2]))
            incomplete_ranking <- dev_matrix[is.na(dev_matrix[,1]) == TRUE, ,drop=FALSE]
            R <- sum(incomplete_ranking[,3])

            # calculate the z score of the rank sum
            i_n <- nrow(incomplete_ranking)
            c_n <- n - i_n
            zr_num <- R - i_n*(i_n+c_n+1)/2
            zr_den <- sqrt(i_n*c_n*(i_n + c_n + 1)/12)
            zr <- zr_num/zr_den

            if (zr > 0) {
              p_zr <- 2*(1-pnorm(zr))
            } else {
              p_zr <- 2*pnorm(zr)
            }

            zr <- round(zr, 3)
            p_zr <- round(p_zr, 3)

            new_row <- c(jj, variable, i_n, zr, p_zr)
            zr_table <- rbind(zr_table, new_row)

        }





        S <- cov(group, use='pairwise')
        col_means <- colMeans(group, na.rm=TRUE)
        D2 <- mahalanobis(group,col_means,S)
        a2p <- sum(D2^2, na.rm=TRUE)

        p <- ncol(group)
        n <- nrow(group)
        E <- n - p*(p + 2)
        sd <- sqrt(8*p*(p + 2))
        M2 <- (a2p/E)/sd

        if (M2 > 0) {
            p_m2 <- 2*(1-pnorm(M2))
        } else {
            p_m2 <- 2*pnorm(M2)
        }
        M2 <- round(M2, 3)
        p_m2 <- round(p_m2, 3)

        new_row <- c(jj, M2, p_m2)
        m2_table <- rbind(m2_table, new_row)

    }

    return(list(zr_table, m2_table))

}
