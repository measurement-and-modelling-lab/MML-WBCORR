function (data, NList, hypothesis, datatype, estimationmethod, deletion) {


    ## Import functions
    adfCov <- dget("./wbcorr/adfCov.R")
    assess_mvn <- dget("./wbcorr/assess_mvn.R")
    assess_range <- dget("./wbcorr/assess_range.R")
    compute4thOrderMoments <- dget("./wbcorr/compute4thOrderMoments.R")
    ComputeWLS <- dget("./wbcorr/ComputeWLS.R")
    errorcheck <- dget("./wbcorr/errorcheck.R")
    GetVecR <- dget("./wbcorr/GetVecR.R")
    MultivariateSK <- dget("./wbcorr/MultivariateSK.R")
    nCov <- dget("./wbcorr/nCov.R")
    pairwiseMVN <- dget("./wbcorr/pairwiseMVN.R")
    fisherTransform <- dget("./wbcorr/fisherTransform.R")


    ## Get the number of samples
    data.length <- length(data)

    
    ## If the upper triangle of a correlation matrix is empty, make the matrix symmetric
    ## Otherwise, check whether the matrix is symmetric and if not return an error
    if (datatype == "correlation") {
        for (i in 1:data.length) {
            groupi <- data[[i]]
            current.upper.triangle <- groupi[upper.tri(groupi)]
            symmetric.upper.triangle <- t(groupi)[upper.tri((groupi))]
            if (all(is.na(current.upper.triangle))) {
                groupi[upper.tri(groupi)] <- symmetric.upper.triangle
            } else if (!all(current.upper.triangle == symmetric.upper.triangle)) {
                stop("Correlation matrix is not symmetric.")
            }
        }
    }
    

    ## Check for a variety of problems
    errorcheck(data, datatype, hypothesis, deletion)


    ## Renumber parameter tags if a number is skipped
    parameter.tags <- hypothesis[hypothesis[,4] != 0, 4]
    if (length(parameter.tags) != 0) {
        if (max(parameter.tags) > length(unique(parameter.tags))) {
            hypothesis[hypothesis[,4] != 0, 4] <- as.numeric(as.factor(parameter.tags))
        }
    }


    ## Apply listwise deletion, if requested
    if (deletion == 'listwise') {
        data <- lapply(data, function(x) {
            temp <- suppressWarnings(as.numeric(x)) ## Convert anything that can't be interpreted as a number to NA
            temp <- matrix(temp, nrow=nrow(x), ncol=ncol(x)) ## as.numeric() converts the matrix into a list, unfortunately
            x <- temp[complete.cases(temp),] ## Delete rows with NAs
        })
    }


    ## Produce the correlation matrix from raw data, with pairwise deletion if requested
    if (datatype == 'rawdata') {
        RList <- list()
        moments <- list()
        data.is.missing <- FALSE
        for (jj in 1:data.length){
            if (deletion == "pairwise") {
                temp <- suppressWarnings(as.numeric(data[[jj]])) ## Convert anything that can't be interpreted as a number to NA
                if (NA %in% temp) {
                    data.is.missing <- TRUE
                }
                data[[jj]] <- matrix(temp, nrow=nrow(data[[jj]])) ## as.numeric() converts the matrix into a list, unfortunately
            }
            moments[[jj]] <- compute4thOrderMoments(data, jj)
            RList[[jj]] <- cor(data[[jj]], use="pairwise") ## If they have no missing data or are using listwise then pairwise doesn't do anything
        }
        if (deletion == "pairwise" & !data.is.missing) { ## Because the pairwise MVN check is an empty matrix with missing data
            stop("You can't use pairwise deletion without missing data.")
        }
    } else {
        RList <- data ## If the data is correlational then do nothing
    }


    ## Check that the correlation matrices are positive definite
    for (i in 1:data.length) {
        eigen_values <- eigen(RList[[i]])[[1]]
        if (TRUE %in% (eigen_values <= 0)) {
            if (deletion == 'pairwise') {
                stop('Data matrix is not positive definite.')
            }
        }
    }


    ## Assess multivariate normality using Yuan, Lambert & Fouladi (2004) if using pairwise deletion, Mardia (1970) otherwise
    if (datatype == 'rawdata') {
        if (deletion == 'pairwise') {
            temp <- assess_range(data)
            MardiaSK <- list(temp[[1]], assess_mvn(data))
            missing <- temp[[2]]
        } else {
            MardiaSK <- MultivariateSK(data)
        }
    } else {
        MardiaSK <- NA
    }
    

    ## Create a list of the correlations referenced in the hypothesis matrix
    VecR <- GetVecR(RList, hypothesis)

    hypothesis.rows <- nrow(hypothesis)


    ## Amend N to match the deletion method
    if (deletion == 'pairwise') {
        NList <- sapply(1:data.length, simplify=TRUE, function(i) {
            groupi <- data[[i]]
            ns <- colSums(!is.na(groupi))
            hmean <- 1/mean(1/ns)
            hmean <- round(hmean, 1) ## Should this really be rounded?
            hmean
        })
    } else if (deletion == 'listwise') {
        NList <- sapply(data, simplify=TRUE, function(group.i) {
            nrow(group.i)
        })
    }


    ## Create a matrix with the sample sizes for the groups referenced in the rows of the hypothesis matrix along its diagonal
    nVector <- NList[hypothesis[,1]] - 1
    nMatrix <- diag(nVector, nrow=length(nVector))


    ## Check if there are any parameter tags; if there are, create delta
    delta <- sapply(1:max(hypothesis[,4]), function(p) {
        as.numeric(p == hypothesis[,4])
    })
    no.parameters <- max(hypothesis[,4]) == 0



    ## Create a vector of fixed values
    hypothesis[hypothesis[,4] != 0, 5] <- 0
    rhostar <- hypothesis[,5,drop=FALSE]


    ## Create WLS (OLS?) estimates
    if (no.parameters) {
        WLSVec <- rhostar
    } else {
        gammahatWLS <- ComputeWLS(VecR,delta,rhostar,nMatrix)
        WLSVec <- delta%*%gammahatWLS + rhostar
    }


    ## Create OLS matrices from WLS (OLS?) estimates
    RWLSList <- RList
    for (kk in 1:hypothesis.rows) {
        a <- hypothesis[kk,1]
        i <- hypothesis[kk,2]
        j <- hypothesis[kk,3]
        RWLSList[[a]][i,j] <- WLSVec[kk,1]
        RWLSList[[a]][j,i] <- WLSVec[kk,1]
    }


    ## Create correlation covariance matrix
    Psi <- matrix(0, nrow=hypothesis.rows, ncol=hypothesis.rows)
    for (jj in 1:hypothesis.rows) {
        for (kk in 1:jj) {
            a <- hypothesis[jj,1]
            i <- hypothesis[jj,2]
            j <- hypothesis[jj,3]
            b <- hypothesis[kk,1]
            k <- hypothesis[kk,2]
            h <- hypothesis[kk,3]
            if (estimationmethod == "TSGLS") {
                Psi[jj,kk] <- nCov(a, i, j, b, k, h, RWLSList)
            } else if (estimationmethod == "GLS") {
                Psi[jj,kk] <- nCov(a, i, j, b, k, h, RList)
            } else if (estimationmethod == "ADF") {
                Psi[jj,kk] <- adfCov(a, i, j, b, k, h, RList, moments)
            } else if (estimationmethod == "TSADF") {
                Psi[jj,kk] <- adfCov(a, i, j, b, k, h, RWLSList, moments)
            }
            Psi[kk,jj] <- Psi[jj,kk]
        }
    }

    OmegaHatInverse <- sqrt(nMatrix)%*%solve(Psi)%*%sqrt(nMatrix)

    rstar <- VecR - rhostar  ## Subtract the estimated value from the observed value

    parameters <- unique(hypothesis[, 4])
    parameters <- parameters[parameters > 0]
    parameters.length <- length(parameters)

    if (no.parameters) {
        e <- rstar
        df <- hypothesis.rows
    } else {
        Psi <- t(delta)%*%OmegaHatInverse
        covgamma <- solve(Psi%*%delta)
        gammahatGLS <- covgamma%*%Psi%*%rstar
        e <- rstar-delta%*%gammahatGLS
        df <- hypothesis.rows - parameters.length
	


	## Produce confidence intervals on parameter estimates (strict Bonferroni)
	gammaGLS_ci <- c()
        for (i in 1:parameters.length) {

            parameter <- parameters[i]
            corrected_alpha <- 0.05/parameters.length
            critical_value <- qnorm(1-corrected_alpha/2)
            parameter.groups <- unique(hypothesis[hypothesis[,4] == parameter, 1]) ## Groups of correlations assigned paramter i
            parameter.sample.sizes <- NList[parameter.groups] ## Sample sizes of the above groups
            N <- sum(parameter.sample.sizes) ## What about the case where you assert the equality of two correlations from one group and one from another?
            point.estimate <- gammahatGLS[i]

            UL <- fisherTransform(point.estimate) + critical_value*sqrt(1/(N-3))
            UL <- tanh(UL)
            UL <- round(UL, 3)
            LL <- fisherTransform(point.estimate) - critical_value*sqrt(1/(N-3))
            LL <- tanh(LL)
            LL <- round(LL, 3)
            gammaGLS_ci[i] <- paste0('[',LL,', ',UL,']')
        }
    }

    temp <- t(e)%*%OmegaHatInverse%*%e
    fGLS <- temp[[1,1]] ## chi square statistic


    ## If there are parameter tags, generate an estimates table
    if (no.parameters == FALSE) {
        gammahatDisplay <- cbind(parameters, gammahatGLS[,1], covgamma[,1])
        gammahatDisplay[,2:3] <- round(gammahatDisplay[,2:3], 3)
        gammahatDisplay <- cbind(gammahatDisplay, gammaGLS_ci)
        confidence.level <- round(100*(1-corrected_alpha), 3)
        colnames(gammahatDisplay) <- c("Parameter Tag", "Estimate", "Standard Error", paste0(confidence.level,'% Confidence Interval'))
        rownames(gammahatDisplay) <- NULL
    } else {
        gammahatDisplay <- NA
    }


    ## Produce significance test results table
    plevel <- 1-pchisq(fGLS,df)
    sigtable <- matrix(round(c(fGLS, df, plevel), 3), nrow=1, ncol=3)
    colnames(sigtable) <- c("Chi Square", "df", "plevel")
    rownames(sigtable) <- NULL


    ## Return output tables
    output <- list(RList, RWLSList, gammahatDisplay, sigtable, MardiaSK, NList, hypothesis)
    return(output)

}
