function (data, NList, hypothesis, datatype, estimationmethod, deletion) {


    ## Import functions
    adfCov <- dget("./wbcorr/adfCov.R")
    assess_range <- dget("./wbcorr/assess_range.R")
    assess_mvn <- dget("./wbcorr/assess_mvn.R")
    compute4thOrderMoments <- dget("./wbcorr/compute4thOrderMoments.R")
    ComputeWLS <- dget("./wbcorr/ComputeWLS.R")
    errorcheck <- dget("./wbcorr/errorcheck.R")
    findpos <- dget("./wbcorr/findpos.R")
    FRHO <- dget("./wbcorr/FRHO.R")
    GetVecR <- dget("./wbcorr/GetVecR.R")
    MakeSymmetricMatrix <- dget("./wbcorr/MakeSymmetricMatrix.R")
    MultivariateSK <- dget("./wbcorr/MultivariateSK.R")
    nCov <- dget("./wbcorr/nCov.R")
    pairwiseMVN <- dget("./wbcorr/pairwiseMVN.R")
    tablegen <- dget("./wbcorr/tablegen.R")
    z <- dget("./wbcorr/fisherz.R")


    ## Get the number of samples
    A <- length(data)


    ## Check for a variety of problems
    errorcheck(data, datatype, hypothesis, deletion)


    ## Apply listwise deletion, if requested
    if (deletion == 'listwise' & datatype == "rawdata") {
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
        for (jj in 1:A){
            if (deletion == "pairwise") {
                temp <- suppressWarnings(as.numeric(data[[jj]])) ## Convert anything that can't be interpreted as a number to NA
                if (NA %in% temp) {
                    data.is.missing <- TRUE
                }
                data[[jj]] <- matrix(temp, nrow=nrow(data[[jj]])) ## as.numeric() converts the matrix into a list, unfortunately
            }
            moments[[jj]] <- compute4thOrderMoments(data, jj)
            RList[[jj]] <- cor(data[[jj]], use="pairwise") ## Since we've already checked for missing data, we can use pairwise in every case
        }
    } else {
        RList <- data ## If the data is already correlation then do nothing
    }

    if (deletion == "pairwise" & !data.is.missing) {
        stop("You can't use pairwise deletion without missing data.")
    }


    ## Check that the correlation matrices are positive definite
    for (i in 1:A) {
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
        NList <- lapply(1:A, function(x) {
            d <- data[[x]]
            ##lowest <- min(colSums(!is.na(d)))
            ns <- colSums(!is.na(d))
            hmean <- 1/mean(1/ns)
            hmean <- round(hmean, 1)
            hmean
        })
    } else if (deletion == 'listwise') {
        NList <- lapply(1:A, function(x) {
            nrow(data[[x]])
        })
    }
    NList <- as.numeric(NList) ## This shouldn't be necessary??


    ## Create a matrix with the sample sizes for the groups referenced in the rows of the hypothesis matrix along its diagonal
    nVector <- NList[hypothesis[,1]] - 1
    nMatrix <- diag(nVector, nrow=length(nVector))


    ## Check if there are any parameter tags; if there are, create delta
    delta <- sapply(1:max(hypothesis[,4]), function(p) as.numeric(p == hypothesis[,4]) )
    nodelta <- max(delta) == 0


    ## Create a vector of fixed values
    hypothesis[hypothesis[,4] != 0, 5] <- 0
    rhostar <- hypothesis[,5]


    ## Create WLS (OLS?) estimates
    if (nodelta == TRUE) {
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
        RWLSList[[a]][[i,j]] <- WLSVec[[kk,1]]
        RWLSList[[a]][[j,i]] <- WLSVec[[kk,1]]
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

    if (nodelta == TRUE) {
        e <- rstar
        parameters.length <- 0
        df <- hypothesis.rows
    } else {
        Psi <- t(delta)%*%OmegaHatInverse
        covgamma <- solve(Psi%*%delta)
        gammahatGLS <- covgamma%*%Psi%*%rstar
        e <- rstar-delta%*%gammahatGLS
        parameters.length <- max(hypothesis[,4])
        df <- hypothesis.rows - parameters.length
	
	
	## Produce confidence intervals on parameter estimates (strict Bonferroni)
	number_of_parameters <- length(gammahatGLS)
	corrected_alpha <- 0.05/number_of_parameters
	critical_value <- qnorm(1-corrected_alpha/2)
	ptags <- unique(hypothesis[,4])
	ptags <- ptags[ptags != 0]
	gammaGLS_ci <- ptags
	for (p in ptags) {
            oneparameterhypothesis <- hypothesis[hypothesis[,4] == p,, drop=FALSE]
            group_column <- oneparameterhypothesis[,1]
            if (max(group_column) == min(group_column)) {

                groupnumber <- max(group_column)
                groupn <- NList[[groupnumber]]
                x <- gammahatGLS[p]
                UL <- z(x) + critical_value*sqrt(1/(groupn-3))
                UL <- tanh(UL)
                UL <- round(UL, 3)
                LL <- z(x) - critical_value*sqrt(1/(groupn-3))
                LL <- tanh(LL)
                LL <- round(LL, 3)

                gammaGLS_ci[p] <- paste0('[',LL,', ',UL,']')

            } else {
                gammaGLS_ci[p] <- ''
            }
	}
    }

    temp <- t(e)%*%OmegaHatInverse%*%e
    fGLS <- temp[[1,1]] ## chi square statistic


    ## If there are parameter tags, generate an estimates table
    if (parameters.length > 0) {
        gammahatDisplay <- matrix(0, nrow=parameters.length, ncol=3)
        parameters <- unique(hypothesis[, 4])
        parameters <- parameters[parameters > 0]
        for (i in 1:parameters.length) {
            gammahatDisplay[i,1] <- round(parameters[i],3)
            gammahatDisplay[i,2] <- round(gammahatGLS[i,1],3)
            gammahatDisplay[i,3] <- round(sqrt(covgamma[i,i]),3)
            if (gammahatDisplay[i,3] == 0) {
                gammahatDisplay[i,3] <- '< 0.001'
            }
        }
        gammahatDisplay <- cbind(gammahatDisplay, gammaGLS_ci)
        colnames(gammahatDisplay) <- c("Parameter Tag", "Estimate", "Standard Error", paste0(100*(1-corrected_alpha),'% Confidence Interval'))
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
