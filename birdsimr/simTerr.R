terrSim <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, # args for bird dataset
                     Nterr, probQual = c(0.1, 0.3, 0.6), #args for terr dataset
                     pMate, # The only externel arg needed for initialize bird on terr function
                     pFidel = 0, pObs = 1, # args still needed for new mate function
                     maxFledge # argument needed for the make fledge function
                     ) {
  dfTerr <- createTerr(Nterr, probQual)
  dfBird <- createBirds(Nbird, maleRatio, Nyr, avgLifespan, propNew)
  birdTerrY1 <- initializeBirdsOnTerr(dfTerr, dfBird, pMate, year = 1)
  birdTerrY1 <- makeFledge(birdTerrY1, maxFledge)
  dfSim <- birdTerrY1
  for (i in 1:(Nyr - 1)) {
    birdTerrY <- dfSim[dfSim$Yr == i, ]
    birdTerrY <- newMate(birdTerrY, dfBird, dfTerr, pFidel, i, pObs, pMate)
    birdTerrY <- makeFledge(birdTerrY, maxFledge)
    dfSim <- rbind(dfSim, birdTerrY)
  }
  return(dfSim)
}
Nbird <- 20
maleRatio <- 0.5
Nyr <- 5
avgLifespan <- 3
propNew <- 0.3
Nterr <- 40
probQual <- c(0.1, 0.3, 0.6)
pFidel <- 0.5
pMate <- 0.9
pObs = 0.95
maxFledge = 4
terrSim(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, Nterr, probQual, 
                    pMate, pFidel = 0, pObs = 1, maxFledge)
