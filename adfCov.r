function (a,i,j,b,k,h,R,moments) {

    FRHO <- dget("FRHO.r")

    if (a != b) {
        Cov <- 0
    } else {

        Cov <- FRHO(i,j,k,h,moments[[a]]) + 1/4*R[[a]][[i,j]]*R[[a]][[k,h]]*(FRHO(i,i,k,k,moments[[a]]) + FRHO(j,j,k,k,moments[[a]]) + FRHO(i,i,h,h,moments[[a]]) + FRHO(j,j,h,h,moments[[a]])) - 1/2*R[[a]][[i,j]]*(FRHO(i,i,k,h,moments[[a]]) + FRHO(j,j,k,h,moments[[a]])) - 1/2*R[[a]][[k,h]]*(FRHO(i,j,k,k,moments[[a]]) + FRHO(i,j,h,h,moments[[a]]))
    }
    return(Cov)
}
