# files needed to run the simulation 
files <- c("birdFunc.R", "createTerr.R", "initializeBirdsOnTerrs.R", 
           "makeFledge.R", "newMate.R")
# load in the functions from the files. 
lapply(files, source)
terrSim <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew, # args for bird dataset
                     Nterr, probQual = c(0.1, 0.3, 0.6), aBad = 1, bBad = 3, 
                    aMid = 1, bMid = 1, aGood = 2, bGood = 1, #args for terr dataset
                     pMate, # The only externel arg needed for initialize bird on terr function
                     pFidel = 0, pDispP = 0.5, 
                    pDispM = 0.8, pDispF = 0.8, # args still needed for new mate function
                     maxFledge,  # argument needed for the make fledge function
                     pObsM = 1, pObsF = 1)
  {
  dfTerr <- createTerr(Nterr, probQual, aBad, bBad, 
                       aMid, bMid, aGood, bGood)
  dfBird <- createBirds(Nbird, maleRatio, Nyr, avgLifespan, propNew)
  birdTerrY1 <- initializeBirdsOnTerr(dfTerr, dfBird, pMate, year = 1)
  birdTerrY1 <- makeFledge(birdTerrY1, maxFledge)
  dfSim <- birdTerrY1
  for (i in 1:(Nyr - 1)) {
    print(i)
    if (length(dfBird[dfBird$Yr == i + 1,]$Sex) == 0) {
      warning("No remaining birds to pull from. Could not complete simulation for 
      all requested years. Try upping the number of available birds.")
      return(dfSim)
    }
    birdTerrY <- dfSim[dfSim$Yr == i, ][, -9]
    birdTerrY <- newMate(birdTerrY, dfBird, dfTerr, pFidel, i, pMate, pDispP, 
                         pDispM, pDispF)
    birdTerrY <- makeFledge(birdTerrY, maxFledge)
    males <- birdTerrY[birdTerrY$Sex == "M", ]
    females <- birdTerrY[birdTerrY$Sex == "F", ]
    males$obs <- rbinom(nrow(males), 1, pObsM)
    females$obs <- rbinom(nrow(females), 1, pObsF)
    males <- males[males$obs == 1, ]
    females <- females[females$obs == 1, ]
    birdTerrY <- rbind(females, males)
    birdTerrY <- birdTerrY[, -10]
    dfSim <- rbind(dfSim, birdTerrY)
  }
  return(dfSim)
}
