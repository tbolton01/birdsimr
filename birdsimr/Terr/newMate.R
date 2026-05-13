newMate <- function(BTYdf, dfBird, dfTerr, pFidel = 0, year, pObs = 1, pMate) {
  # gives df of birds available for the next year
  birdYear <- dfBird[dfBird$Yr == (year + 1),]
  # Removing some birds for the year randomly to replicate that 
  # sometimes birds aren't seen in a particular year but are still alive
  birdYear$Obs <- rbinom(length(birdYear$Yr), 1, pObs)
  #Removing all birds that weren't seen
  birdYear <- birdYear[birdYear$Obs == 1, ]
  # were going to throw out birds that either are dead or wern't observed.
  BTYdf <- BTYdf[which(BTYdf$birdID %in% birdYear$birdID), ]
  matedMales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "M", ]
  matedFemales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "F", ]
  # We now need to subset the groups above and make sure they're mate is still alive
  matedFemales <- matedFemales[which(matedFemales$Terrs %in% matedMales$Terrs), ]
  matedMales <- matedMales[which(matedMales$Terrs %in% matedFemales$Terrs), ]
  # We now have 2 datasets will all remaining mated males and females
  # If the mated column becomes a 0, the female will be eligble for a new mate.
  matedFemales$Mated <- rbinom(length(matedFemales$Mated), 1, pFidel)
  # Here is a df with the females who will stay with their mate
  matedFemales <- matedFemales[matedFemales$Mated == 1, ]
  # This line removes the males from the mated males who were separated from 
  # their mate
  matedMales <- matedMales[which(matedMales$Terrs %in% matedFemales$Terrs),]
  matedBird <- rbind(matedMales, matedFemales)
  # We now have the final list of birds available for the year
  birdYear <- birdYear[which(!birdYear$birdID %in% matedBird$birdID), ]
  birdYear <- birdYear[, -5]
  dfTerr <- dfTerr[which(!as.numeric(dfTerr$terr) %in% as.numeric(matedBird$Terrs)), ]
  newMate <- initializeBirdsOnTerr(dfTerr, birdYear, pMate, year = (year + 1))
  newMate$Fledge <- rep(0, length(newMate$Yr))
  matedBird$Fledge <- rep(0, length(matedBird$Yr))
  matedBird$Yr <- as.numeric(matedBird$Yr) + 1
  df <- rbind(matedBird, newMate)
  return(df)
}
newMate(BTYdf, birds, territories, pFidel = 0.8, year = 1, pMate = 0.6)
