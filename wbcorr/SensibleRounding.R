function (number, digits) {

    number[abs(number)>0.001] <- round(number[abs(number)>0.001], 3)
    number[abs(number)<0.001] <- format(number[abs(number)<0.001], scientific=TRUE, digits=1)
    number[number==1] <- ">0.999"
    return(number)

}
