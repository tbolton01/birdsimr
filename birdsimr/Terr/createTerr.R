createTerr <- function(Nterr, probQaul = c(0.1, 0.3, 0.6)){
  terrVec <- 1:Nterr
  qualVec <- sample(c("good", "mid", "bad"), Nterr, replace = TRUE, prob = probQaul)
  goodIndices <- which(qualVec == "good")
  midIndices <- which(qualVec == "mid")
  badIndices <- which(qualVec == "bad")
  probOcc <- rep(0, Nterr)
  probFledge <- rep(0, Nterr)
  probOcc[goodIndices] <- rbeta(n = length(goodIndices), 3, 1)
  probFledge[goodIndices] <- rbeta(n = length(goodIndices), 3, 1)
  probOcc[midIndices] <- runif(n = length(midIndices), 0, 1)
  probFledge[midIndices] <- runif(n = length(midIndices), 0, 1)
  probOcc[badIndices] <- rbeta(n = length(badIndices), 1, 3)
  probFledge[badIndices] <- rbeta(n = length(badIndices), 1, 3)
  dfMatrix <- cbind(terrVec, qualVec, probOcc, probFledge)
  df <- data.frame(dfMatrix)
  colnames(df) <- c("terr", "quality", "Poccup", "Pfledge")
  return(df)
}

# Hello
# Checking from VS