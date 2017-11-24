function (data, NList, hypothesis, datatype, estimationmethod, deletion) {

# Import functions
adfCov <- dget("adfCov.r")
compute4thOrderMoments <- dget("compute4thOrderMoments.r")
ComputeWLS <- dget("ComputeWLS.r")
errorcheck <- dget("errorcheck.r")
findpos <- dget("findpos.r")
FRHO <- dget("FRHO.r")
GetVecR <- dget("GetVecR.r")
MakeDeltaFromHypothesis <- dget("MakeDeltaFromHypothesis.r")
MakeNMatrix <- dget("MakeNMatrix.r")
MakeRhoStarFromHypothesis <- dget("MakeRhoStarFromHypothesis.r")
MakeSymmetricMatrix <- dget("MakeSymmetricMatrix.r")
MultivariateSK <- dget("MultivariateSK.r")
nCov <- dget("nCov.r")
pairwiseMVN <- dget("pairwiseMVN.R")
tablegen <- dget("tablegen.r")

assess_range <- dget("assess_range.R")
assess_mvn <- dget("assess_mvn.R")

A = length(data)


# Make the matrix symmetric if it's square, give error otherwise
if (datatype == 'correlation') {
    for (i in 1:A) {
        if (nrow(data[[i]]) == ncol(data[[i]])) {
            data[[i]] <- MakeSymmetricMatrix(data[[i]])
        } else {
            cat('<br>Error: Data matrix #', jj, ' is not square.', sep="")
            return(invisible(TRUE))
        }
    }
}



if (deletion == 'listwise') { # apply listwise deletion
    data <- lapply(data, function(x) {
        temp1 <- suppressWarnings(as.numeric(x))
        temp1 <- matrix(temp1, nrow=nrow(x), ncol=ncol(x))
        x <- temp1[complete.cases(temp1),]
    })
}


error <- errorcheck(data, datatype, hypothesis,deletion)
if (error == TRUE) {
  return(invisible())
}

hypothesis <- hypothesis [order(hypothesis[,4]), , drop=FALSE]
parametertags <- unique(hypothesis[,4])

if (parametertags != c(0)) {

  parametertags <- parametertags[parametertags != 0]
  num <- length(parametertags)
  
  for (p in 1:num) {
    if (parametertags[[p]] != p) {
      hypothesis[,4] <- unlist(lapply(hypothesis[,4], function(x) {
        if (x == parametertags[[p]]) {
          x <- p
        } else {
          x <- x
        }
      }))
    }
  }
}



# Produce the correlation matrix using raw data, with pairwise deletion if requested
if (datatype == 'rawdata'){
    RList <- list()
    moments <-list()
    for (jj in 1:A){
        temp <- suppressWarnings(as.numeric(data[[jj]]))
        data[[jj]] <- matrix(temp, nrow=nrow(data[[jj]]), ncol=ncol(data[[jj]]))
        if (deletion == 'pairwise' && NA %in% temp) {
          RList[[jj]] <- cov(data[[jj]], use='pairwise')
        } else {
          RList[[jj]] <- cov(data[[jj]])
        }
        RList[[jj]] <- cov2cor(RList[[jj]])
        moments[[jj]] <- compute4thOrderMoments(data, jj)
    }
} else {
	RList <- data
}

# Check that the correlation matrices are positive definite
for (i in 1:A) {
    eigen_values <- eigen(RList[[i]])[[1]]
    if (TRUE %in% (eigen_values <= 0)) {
        if (deletion == 'pairwise') {
            cat('<br>Error: Data matrix #', jj, ' is not positive definite. Try using listwise deletion instead of pairwise.', sep="")
            return(invisible(TRUE))
        } else {
            cat('<br>Error: Data matrix #', jj, ' is not positive definite.', sep="")
            return(invisible(TRUE))
        }
    }
}

# Assess multivariate normality using Yuan, Lambert & Fouladi (2004) if using pairwise deletion, Mardia (1970) otherwise
if (datatype == 'rawdata') {
    if (deletion == 'pairwise') {
        temp <- assess_range(data)
        MardiaSK <- list(temp[[1]], assess_mvn(data))
        missing <- temp[[2]]
    } else {
	      MardiaSK <- MultivariateSK(data)
    }
}

VecR <- GetVecR(RList, hypothesis)

rows <- nrow(hypothesis)

RRList <- list()
RRList <- lapply(1:A, function(x) {
    RRList[[x]] <- MakeSymmetricMatrix(RList[[x]])
})

# Amend N to match the deletion method
if (deletion == 'pairwise') {
    NList <- lapply(1:A, function(x) {
        d <- data[[x]]
        #lowest <- min(colSums(!is.na(d)))
        ns <- colSums(!is.na(d))
        hmean <- 1/mean(1/ns)
        hmean <- round(hmean, 1)
        NList[[jj]] <- hmean
    })
} else if (deletion == 'listwise') {
    NList <- lapply(1:A, function(x) {
        NList[[x]] <- nrow(data[[x]])
    })
}

nMatrix <- MakeNMatrix(NList, hypothesis)

delta <- MakeDeltaFromHypothesis(hypothesis)

nodelta <- is.null(delta)
rhostar <- MakeRhoStarFromHypothesis(hypothesis)

if (nodelta == TRUE) {
    WLSVec <- rhostar
} else {
    gammahatWLS <- ComputeWLS(VecR,delta,rhostar,nMatrix)
    WLSVec <- delta%*%gammahatWLS + rhostar
}

RWLSList <- RRList

for (kk in 1:rows) {
    a <- hypothesis[[kk,1]]
    i <- hypothesis[[kk,2]]
    j <- hypothesis[[kk,3]]
    RWLSList[[a]][[i,j]] <- WLSVec[[kk,1]]
    RWLSList[[a]][[j,i]] <- WLSVec[[kk,1]]
}

# Replace correlations being assessed against the same fixed value with the mean of those correlations
if (length(hypothesis[hypothesis[,4] == 0,] && estimationmethod %in% c('TSGLS','TSADF')) > 1) {
    fixed_values <- unique(hypothesis[(hypothesis[,4]==0),][,5])
    for (f in fixed_values) {
        sub_hypothesis <- hypothesis[(hypothesis[,5]==f),]
        if (is.matrix(sub_hypothesis)) {
            sum <- 0
            for (r in 1:nrow(sub_hypothesis)) {
                    group <- sub_hypothesis[r,][1]
                    row <- sub_hypothesis[r,][2]
                    column <- sub_hypothesis[r,][3]
                    sum <- sum + data[[group]][[row,column]]
            }
            mean <- sum/nrow(sub_hypothesis)
            for (r in 1:nrow(sub_hypothesis)) {
                    group <- sub_hypothesis[r,][1]
                    row <- sub_hypothesis[r,][2]
                    column <- sub_hypothesis[r,][3]
                    RWLSList[[group]][[row,column]] <- mean
                    RList[[group]][[row,column]] <- mean
            }
        }
    }
    RWLSList <- lapply(RWLSList, function(x) MakeSymmetricMatrix(x))
    RList <- lapply(RList, function(x) MakeSymmetricMatrix(x))
}

Psi <- matrix(0, nrow=rows, ncol=rows)

# For each row of the hypothesis matrix, loop through each preceding row + itself; estimate the covariance of each pair using the requested method
for (jj in 1:rows) {
    for (kk in 1:jj) {
        a <- hypothesis[[jj,1]]
        i <- hypothesis[[jj,2]]
        j <- hypothesis[[jj,3]]
        b <- hypothesis[[kk,1]]
        k <- hypothesis[[kk,2]]
        h <- hypothesis[[kk,3]]
        if (estimationmethod == "TSGLS") {
            Psi[[jj,kk]] <- nCov(a, i, j, b, k, h, RWLSList)
        } else if (estimationmethod == "GLS") {
             Psi[[jj,kk]] <- nCov(a, i, j, b, k, h, RList)
        } else if (estimationmethod == "ADF") {
              Psi[[jj,kk]] <- adfCov(a, i, j, b, k, h, RList, moments)
        } else if (estimationmethod == "TSADF") {
               Psi[[jj,kk]] <- adfCov(a, i, j, b, k, h, RWLSList, moments)
        }
        Psi[[kk,jj]] <- Psi[[jj,kk]]
    }
}

OmegaHatInverse <- sqrt(nMatrix)%*%solve(Psi)%*%sqrt(nMatrix)

rstar <- VecR - rhostar  # Subtract the estimated value from the observed value

if (nodelta == TRUE) {
    e <- rstar
    pars <- 0
    df <- rows
} else {
    Psi <- t(delta)%*%OmegaHatInverse
    covgamma <- solve(Psi%*%delta)
    gammahatGLS <- covgamma%*%Psi%*%rstar
    e <- rstar-delta%*%gammahatGLS
    pars <- nrow(gammahatGLS)
    df <- rows-pars
}

temp <- t(e)%*%OmegaHatInverse%*%e
fGLS <- temp[[1,1]]

hr <- function () {
	cat('\n<div style="width: 100%; height: 3px; background: #717171; overflow: hidden;">-----------</div>\n')
}

printfunction <- function () {
    cat('<br><div style="line-height: 75%;"><b>Hypothesis Matrix</b>\n\n</div>')
    
    hypothesis_table <- rbind(c("Group", "Row", "Column", "Parameter Tag", "Fixed Value"), hypothesis)
    tablegen(hypothesis_table,TRUE)
    
    hr()
    for(i in 1:A){

        cat(paste('<div style="line-height: 75%;"><b>Input Correlation Matrix #', i, ' (N=', NList[[i]],')</b>\n\n</div>', sep=""))
        tablegen(round(RRList[[i]],3),FALSE)
        cat('\n')
        
        cat(paste('<div style="line-height: 75%;"><b>OLS Estimates of Correlation Matrix #', i, '</b>\n\n</div>', sep=""))
        tablegen(round(RWLSList[[i]],3),FALSE)

        if (i != A) {
            cat('\n\n')
        }

    }
    
    hr()
    
    if (pars > 0) {
        gammahatDisplay <- matrix(0, nrow=pars, ncol=3)
        parlist <- unique(hypothesis[, 4])
        for (i in 1:pars) {
            gammahatDisplay[[i,1]] <- round(parlist[[i]],3)
            gammahatDisplay[[i,2]] <- round(gammahatGLS[[i,1]],3)
            gammahatDisplay[[i,3]] <- round(sqrt(covgamma[[i,i]]),3)
            if (gammahatDisplay[[i,3]] == 0) {
                gammahatDisplay[[i,3]] <- '< 0.001'
            }
        }
        
        cat('<div style="line-height: 75%;">')
        if (estimationmethod == "TSGLS") {
            cat("<b>2-Stage GLS Parameter Estimates</b>\n\n")
        } else if (estimationmethod == "GLS") {
            cat("<b>GLS Parameter Estimates</b>\n\n")
        } else if (estimationmethod == "ADF") {
            cat("<b>ADF Parameters Estimates</b>\n\n")
        } else if (estimationmethod == "TSADF") {
            cat("<b>2-Stage ADF Parameter Estimates</b>\n\n")
        }
        cat('</div>')
        
        gammahatDisplay <- rbind(c("Parameter Tag", "Estimate", "Standard Error"), gammahatDisplay)
        tablegen(gammahatDisplay,TRUE)
    }
    
    plevel <- 1-pchisq(fGLS,df)
    
    if (nodelta == FALSE) {
        cat('<br>')
    }
    
    cat('<div style="line-height: 75%;"><b>Significance Test Results</b>\n\n</div>')
    
    sig_table <- rbind(c("Chi Square", "&nbsp;df&nbsp;", "plevel"), round(c(fGLS, df, plevel),3))
    
    if (sig_table[2,3] == 0) {
        sig_table[2,3] <- '< 0.001'
    }
    
    tablegen(sig_table,TRUE)
  
    if (datatype == 'rawdata' && deletion != 'pairwise') {
      
        hr()
        cat('<div style="line-height: 75%;"><b>Assessment of Multivariate Normality</b>\n\n</div>')
    
        tablegen(MardiaSK[[1]],TRUE)
        cat('\n')
    
        tablegen(MardiaSK[[2]],TRUE)
        cat('\n')
        
    }
    
    if (deletion == 'pairwise') {

        hr()


        
        if (length(missing) > 0) {
          cat('<div style="line-height: 75%;"><b>Assessment of the Distribution of the Observed Marginals*</b>\n\n</div>')
          tablegen(MardiaSK[[1]], TRUE)
          missing <- paste(missing, collapse=", ")
          cat('&nbsp;&nbsp;&nbsp;&nbsp;* - Groups skipped because scores were missing in every column:', missing, '<br><br>')
        } else {
          cat('<div style="line-height: 75%;"><b>Assessment of the Distribution of the Observed Marginals</b>\n\n</div>')
          tablegen(MardiaSK[[1]], TRUE)
          cat('\n')
        }
        
        cat('<div style="line-height: 75%;"><b>Assessment of Multivariate Normality</b>\n\n</div>')
        tablegen(MardiaSK[[2]], TRUE)
        cat('\n')
        

    
    }    

    cat('<br>')
}
x <- capture.output(printfunction())

cat(paste(x, collapse='<br>'))


}
