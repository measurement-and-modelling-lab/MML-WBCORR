function (a,i,j,b,k,h,Rlist) {

    if (a != b) {
        cov <- 0
    } else {
        cov <- (1/2)*Rlist[[a]][[i,j]]*Rlist[[a]][[k,h]]*(Rlist[[a]][[i,k]]^2+Rlist[[a]][[i,h]]^2+Rlist[[a]][[j,k]]^2+Rlist[[a]][[j,h]]^2)+Rlist[[a]][[i,k]]*Rlist[[a]][[j,h]]+Rlist[[a]][[i,h]]*Rlist[[a]][[j,k]]-Rlist[[a]][[i,j]]*(Rlist[[a]][[j,k]]*Rlist[[a]][[j,h]]+Rlist[[a]][[i,k]]*Rlist[[a]][[i,h]])-Rlist[[a]][[k,h]]*(Rlist[[a]][[j,k]]*Rlist[[a]][[i,k]]+Rlist[[a]][[j,h]]*Rlist[[a]][[i,h]])
    }
    return(cov)
}
