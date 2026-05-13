createTerr <- function(Nterr){
  terrVec <- 1:Nterr
  df <- data.frame(terrVec)
  colnames(df) <- c("terr")
  return(df)
}