function (number, digits) {

    number[number>0.00001] <- round(number[number>0.00001], digits)
    number[number<0.00001] <- format(number[number<0.00001], scientific=TRUE, digits=1)
    number[number==1] <- "> 0.99999"
    return(number)

}
