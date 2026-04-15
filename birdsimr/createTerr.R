createTerr <- function(Nterr, probQaul = c(0.1, 0.3, 0.6)){
  terrVec <- 1:Nterr
  qualVec <- sample(c("good", "mid", "bad"), Nterr, replace = TRUE, prob = probQaul)
  goodIndices <- which(qualVec == "good")
  midIndices <- which(qualVec == "mid")
  badIndices <- which(qualVec == "bad")
  probOcc <- rep(0, Nterr)
  probFledge <- rep(0, Nterr)
  probOcc[goodIndices] <- rbeta(n = length(goodIndices), 10, 1)
  probFledge[goodIndices] <- rbeta(n = length(goodIndices), 10, 1)
  probOcc[midIndices] <- runif(n = length(midIndices), 0, 1)
  probFledge[midIndices] <- runif(n = length(midIndices), 0, 1)
  probOcc[badIndices] <- rbeta(n = length(badIndices), 1, 10)
  probFledge[badIndices] <- rbeta(n = length(badIndices), 1, 10)
  dfMatrix <- cbind(terrVec, qualVec, probOcc, probFledge)
  df <- data.frame(dfMatrix)
  colnames(df) <- c("terr", "quality", "P(occup)", "P(fledge)")
  return(df)
}

# Hello
# Checking from VS