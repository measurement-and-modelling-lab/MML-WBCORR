 compute4thOrderMoments <-function( data, jj ) {
    n <- nrow(data[[jj]])
    p <- ncol(data[[jj]])
    means <- matrix(0,p,1)
    sds <- matrix(0,p,1)
    zscores <- data[[jj]]
    means <- apply(data[[jj]],2,mean)
    sds <- apply(data[[jj]],2,sd)

    for (i in 1:p) {
        zscores[,i] <- unlist(lapply(zscores[,i], function(x) (x-means[[i]])/sds[[i]]))
    }


    q <- p*(p+1)*(p+2)*(p+3)/24
    moments <- matrix(0,q,1)
    a <- 0
    for (i in 1:p) {
            for (j in 1:i) {
                    for (k in 1:j) {
                            for (h in 1:k) {
                                    a<-a+1
                                    for (b in 1:n) {
                                            moments[[a]] <- moments[[a]]+zscores[b,i]*zscores[b,j]*zscores[b,k]*zscores[b,h]
                                    } 
                            } 
                    } 
            } 
    }
	moments = moments/(n-1)
    return(moments)
}
