pRound <- function (x) {
    if (x > 0.999) {
        return("> 0.999")
    } else if (x < 0.001) {
        return(format(x, scientific=TRUE, digits=1))
    } else {
        round(x, 3)
    }
}
