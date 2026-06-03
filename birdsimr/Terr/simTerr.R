# files needed to run the simulation 
files <- c("birdFunc.R", "createTerr.R", "initializeBirdsOnTerrs.R", 
           "makeFledge.R", "newMate.R")
# load in the functions from the files. 
lapply(files, source)
terrSim <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, # args for bird dataset
                     Nterr, probQual = c(0.1, 0.3, 0.6), #args for terr dataset
                     pMate, # The only externel arg needed for initialize bird on terr function
                     pFidel = 0, pDispP = 0.6, 
                    pDispM = 0.6, pDispF = 0.6, # args still needed for new mate function
                     maxFledge # argument needed for the make fledge function
                     ) {
  dfTerr <- createTerr(Nterr, probQual)
  dfBird <- createBirds(Nbird, maleRatio, Nyr, avgLifespan, propNew)
  print("Han Solo")
  birdTerrY1 <- initializeBirdsOnTerr(dfTerr, dfBird, pMate, year = 1)
  print("Hello")
  birdTerrY1 <- makeFledge(birdTerrY1, maxFledge)
  dfSim <- birdTerrY1
  for (i in 1:(Nyr - 1)) {
    if (length(dfBird[dfBird$Yr == i + 1,]$Sex) == 0) {
      warning("No remaining birds to pull from. Could not complete simulation for 
      all requested years. Try upping the number of available birds.")
      return(dfSim)
    }
    birdTerrY <- dfSim[dfSim$Yr == i, ][, -9]
    birdTerrY <- newMate(birdTerrY, dfBird, dfTerr, pFidel, i, pMate, pDispP, 
                         pDispM, pDispF)
    print("gravity")
    birdTerrY <- makeFledge(birdTerrY, maxFledge)
    print("Kylo Ren")
    dfSim <- rbind(dfSim, birdTerrY)
  }
  return(dfSim)
}
Nbird <- 100
maleRatio <- 0.5
Nyr <- 5
avgLifespan <- 3
propNew <- rep(0.4, Nyr)
Nterr <- 100
probQual <- c(0.1, 0.3, 0.6)
pFidel <- 0.5
pMate <- 0.9
pObs = 0.95
maxFledge = 4
DF <- terrSim(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, Nterr, probQual, 
                    pMate, pFidel = 0, maxFledge)
