FRHO <- function(i,j,k,h,M){
  findpos <- dget("./wbcorr/findpos.R")
  temp <- findpos(i,j,k,h)
  fpho <- M[temp]
  return(fpho)
}
