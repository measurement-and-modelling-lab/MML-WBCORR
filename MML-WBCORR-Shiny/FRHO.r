FRHO <- function(i,j,k,h,M){
  findpos <- dget("findpos.r")
  temp <- findpos(i,j,k,h)
  fpho <- M[temp]
  return(fpho)
}
