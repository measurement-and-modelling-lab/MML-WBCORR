function (number) {

    if (number < 0.001) {
        number <- format(number, scientific=TRUE, digits=1)
    } else if (number > 0.999) {
        number <- paste0("1 - ", format(1-number, scientific=TRUE, digits=1))
    } else {
        number <- round(number, 3)
    }

    return(number)

}
