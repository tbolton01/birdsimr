# files needed to run the simulation 
files <- c("birdFunc.R", "createTerr.R", "initializeBirdsOnTerrs.R", 
           "makeFledge.R", "newMate.R")
# load in the functions from the files. 
lapply(files, source)
terrSim <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, # args for bird dataset
                    Nterr, #arg for terr dataset
                    pFidel = 0, pObs = 1, # args still needed for new mate function
                    maxFledge # argument needed for the make fledge function
) {
  dfTerr <- createTerr(Nterr)
  dfBird <- createBirds(Nbird, maleRatio, Nyr, avgLifespan, propNew)
  birdTerrY1 <- initializeBirdsOnTerr(dfTerr, dfBird, year = 1)
  birdTerrY1 <- makeFledge(birdTerrY1, maxFledge)
  dfSim <- birdTerrY1
  for (i in 1:(Nyr - 1)) {
    birdTerrY <- dfSim[dfSim$Yr == i, ]
    birdTerrY <- newMate(birdTerrY, dfBird, dfTerr, pFidel, i, pObs)
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
pFidel <- 0.5
pObs = 0.95
maxFledge = 4
terrSim(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, Nterr, pFidel = 0, pObs = 1, maxFledge)
