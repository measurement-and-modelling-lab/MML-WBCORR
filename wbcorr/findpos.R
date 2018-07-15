function(i, j, k, h) {

    temp <- c(i, j, k, h)
    list <- sort(temp, decreasing = TRUE)

    a <- list[[1]]
    b <- list[[2]]
    c <- list[[3]]
    d <- list[[4]]
    post <- (a-1)*a*(a+1)*(a+2)/24 + (b-1)*b*(b+1)/6 + c*(c-1)/2 + d

    return(post)
}
