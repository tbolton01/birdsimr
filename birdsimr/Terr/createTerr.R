createTerr <- function(Nterr, probQaul = c(0.1, 0.3, 0.6), aBad = 1, bBad = 3, 
                       aMid = 1, bMid = 1, aGood = 2, bGood = 1){
  terrVec <- 1:Nterr
  qualVec <- sample(c("good", "mid", "bad"), Nterr, replace = TRUE, prob = probQaul)
  goodIndices <- which(qualVec == "good")
  midIndices <- which(qualVec == "mid")
  badIndices <- which(qualVec == "bad")
  probOcc <- rep(0, Nterr)
  probFledge <- rep(0, Nterr)
  probOcc[goodIndices] <- rbeta(n = length(goodIndices), aGood, bGood)
  probFledge[goodIndices] <- rbeta(n = length(goodIndices), aGood, bGood)
  probOcc[midIndices] <- rbeta(n = length(midIndices), aMid, bMid)
  probFledge[midIndices] <- rbeta(n = length(midIndices), aMid, bMid)
  probOcc[badIndices] <- rbeta(n = length(badIndices), aBad, bBad)
  probFledge[badIndices] <- rbeta(n = length(badIndices), aBad, bBad)
  dfMatrix <- cbind(terrVec, qualVec, probOcc, probFledge)
  df <- data.frame(dfMatrix)
  colnames(df) <- c("terr", "quality", "Poccup", "Pfledge")
  return(df)
}