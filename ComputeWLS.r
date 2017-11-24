ComputeWLS <- function(VecR,Delta, RhoStar, nMatrix) {
    X <- solve(t(Delta)%*%nMatrix%*%Delta)%*%t(Delta)%*%nMatrix
    Y <- VecR-RhoStar
    X <- X%*%Y
    return(X)
}
