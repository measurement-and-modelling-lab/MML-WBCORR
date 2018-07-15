function (data) {
    
    missing <- c()

    zr_table <- list()
    
    for (jj in 1:length(data)) {

        group <- data[[jj]]
        n <- nrow(group)
        colnames(group) <- 1:ncol(group)

        ## isolate columns with and without missing scores
        incomplete <- group[,colSums(is.na(group)) > 0, drop=FALSE]
        complete <- group[,colSums(is.na(group)) == 0, drop=FALSE]
        
        if (length(incomplete) == 0) {
            next
        }
        if (length(complete) == 0) {
            missing <- c(missing, jj)
            next
        }

        while (length(incomplete)*length(complete) > 0) {

            pair <- cbind(incomplete[,1], complete[,1])
            variable <- colnames(incomplete)[[1]]
            incomplete <- incomplete[,-1]

            ## take the absolute deviation of each score from the mean of its column
            m <- colMeans(pair, na.rm=TRUE)
            m_matrix <- matrix(rep(m, each=n), nrow=n, ncol=2)
            dev_matrix <- abs(pair - m_matrix)

            ## rank the incomplete scores by the value of the corresponding complete scores, then sum the ranks of the NA values
            dev_matrix <- cbind(dev_matrix, rank(dev_matrix[,2]))
            incomplete_ranking <- dev_matrix[is.na(dev_matrix[,1]) == TRUE, ,drop=FALSE]
            R <- sum(incomplete_ranking[,3])

            ## calculate the z score of the rank sum
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
            zr_table[[jj]] <- new_row

        }
    }

    zr_table <- do.call(rbind, zr_table)
    colnames(zr_table) <- c('Group','Variable','Missing values','Z<sub>R</sub>', 'plevel (two-tail)')

    return(list(zr_table, missing))
}
