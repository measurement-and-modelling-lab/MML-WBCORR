function (number) {

    number <- round(number, 3)
    number[number==0] <- "< 0.001"
    number[number==1] <- "> 0.999"

    return(number)

}
