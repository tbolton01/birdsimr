newMate <- function(BTYdf, dfBird, dfTerr, pFidel = 0, year, pMate, pDispP = 0.6, 
                    pDispM = 0.6, pDispF = 0.6) {
  males <- dfBird[dfBird$Sex == "M" & dfBird$Yr == (year + 1), ]
  females <- dfBird[dfBird$Sex == "F" & dfBird$Yr == (year + 1), ]
  nMale <- nrow(males)
  nFemale <- nrow(females)
  nTerr <- nrow(dfTerr)
  if (nFemale > nTerr | nMale > nTerr) {
    stop("Cannot have more birds than territories")
  }
  # gives df of birds available for the next year
  birdYear <- dfBird[dfBird$Yr == (year + 1),]
  # were going to throw out birds that either are dead or wern't observed.
  BTYdf <- BTYdf[which(BTYdf$birdID %in% birdYear$birdID), ]
  BTYdf$Yr <- rep(year + 1, nrow(BTYdf))
  oldMatedMales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "M", ]
  oldMatedFemales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "F", ]
  # We now need to subset the groups above and make sure they're mate is still alive
  matedFemales <- oldMatedFemales[which(oldMatedFemales$Terrs %in% oldMatedMales$Terrs), ]
  matedMales <- oldMatedMales[which(oldMatedMales$Terrs %in% oldMatedFemales$Terrs), ]
  # We now have 2 datasets will all remaining mated males and females
  # If the mated column becomes a 0, the female will be eligible for a new mate.
  matedFemales$Mated <- rbinom(length(matedFemales$Mated), 1, pFidel)
  # Here is a df with the females who will stay with their mate
  matedFemales <- matedFemales[which(matedFemales$Mated == 1), ]
  # This line removes the males from the mated males who were separated from 
  # their mate 
  matedMales <- matedMales[which(matedMales$Terrs %in% matedFemales$Terrs),]
  matedBird <- rbind(matedMales, matedFemales)
  BTYdf <- BTYdf[which(!BTYdf$birdID %in% matedBird$birdID), ]
  BTYdf$Mated <- rep(0, nrow(BTYdf))
  BTYdf <- rbind(BTYdf, matedBird)
  singleMales <- BTYdf[BTYdf$Sex == "M" & BTYdf$Mated == 0, ]
  singleFemales <- BTYdf[BTYdf$Sex == "F" & BTYdf$Mated == 0, ]
  matedMales$disp <- rbinom(nrow(matedMales), 1, pDispP)
  matedFemales$disp <- as.numeric(matedMales$disp[match(matedFemales$Terrs, matedMales$Terrs)])
  singleMales$disp <- rbinom(nrow(singleMales), 1, pDispM)
  singleFemales$disp <- rbinom(nrow(singleFemales), 1, pDispF)
  singleMales[which(singleMales$Terrs %in% oldMatedMales$Terrs), ]$disp <- 1
  singleFemales[which(singleFemales$Terrs %in% oldMatedFemales$Terrs), ]$disp <- 1
  BTYdf <- rbind(matedMales, matedFemales, singleMales, singleFemales)
  BTYdf$oldTerrs <- BTYdf$Terrs
  BTYdf[BTYdf$disp == 0, ]$Terrs <- BTYdf[BTYdf$disp == 0, ]$oldTerrs
  BTYdf[BTYdf$disp == 1, ]$Terrs <- NA
  BTYdf[BTYdf$disp == 1, ]$Poccup <- NA
  BTYdf[BTYdf$disp == 1, ]$Pfledge <- NA
  newBirds <- birdYear[which(!birdYear$birdID %in% BTYdf$birdID), ]
  newBirds$Mated <- 0
  newBirds$Terrs <- NA
  newBirds$Poccup <- NA
  newBirds$Pfledge <- NA
  newBirds$disp <- 1
  newBirds$oldTerrs <- 0
  BTYdf <- rbind(BTYdf, newBirds)
  maleSingDisp <- BTYdf[BTYdf$Sex == "M" & BTYdf$Mated == 0 & BTYdf$disp == 1, ]
  femaleSingDisp <- BTYdf[BTYdf$Sex == "F" & BTYdf$Mated == 0 & BTYdf$disp == 1, ]
  # We first assign pairs to empty territories
  if(sum(BTYdf[BTYdf$Mated == 1, ]$disp) > 0) {
    emptyTerrs <- dfTerr[which(!dfTerr$terr %in% BTYdf$Terrs), ]
    maleMatedDisp <- BTYdf[BTYdf$Sex == "M" & BTYdf$Mated == 1 & BTYdf$disp == 1, ]
    maleMatedDisp$Terrs <- sample(emptyTerrs$terr, nrow(maleMatedDisp), 
                                 prob = as.numeric(emptyTerrs$Poccup), 
                                 replace = FALSE)
    maleMatedDisp$Poccup <- as.numeric(dfTerr$Poccup[match(maleMatedDisp$Terrs, dfTerr$terr)])
    maleMatedDisp$Pfledge <- as.numeric(dfTerr$Pfledge[match(maleMatedDisp$Terrs, dfTerr$terr)])
    femaleMatedDisp <- BTYdf[BTYdf$Sex == "F" & BTYdf$Mated == 1 & BTYdf$disp == 1, ]
    femaleMatedDisp$Terrs <- maleMatedDisp$Terrs[match(maleMatedDisp$oldTerrs, femaleMatedDisp$oldTerrs)]
    femaleMatedDisp$Poccup <- as.numeric(maleMatedDisp$Poccup[match(femaleMatedDisp$oldTerrs, maleMatedDisp$oldTerrs)])
    femaleMatedDisp$Pfledge <- as.numeric(maleMatedDisp$Pfledge[match(maleMatedDisp$oldTerrs, femaleMatedDisp$oldTerrs)])
    BTYdf <- na.omit(BTYdf)
    BTYdf <- rbind(BTYdf, maleMatedDisp, femaleMatedDisp)
  }
  # we now send the single males off to territories that have dispersed or were introduced this year
  if(nrow(maleSingDisp) > 0) {
    BTYdf <- na.omit(BTYdf)
    emptyTerrs <- dfTerr[which(!dfTerr$terr %in% BTYdf$Terrs), ]
    sfTerrs <- dfTerr[which(dfTerr$terr %in% BTYdf[BTYdf$Sex == "F" & BTYdf$disp == 0 & BTYdf$Mated == 0, ]$Terrs), ]
    availTerrs <- rbind(emptyTerrs, sfTerrs)
    maleSingDisp$Terrs <- sample(availTerrs$terr, nrow(maleSingDisp), prob = as.numeric(availTerrs$Poccup), replace = FALSE)
    maleSingDisp$Poccup <- as.numeric(dfTerr$Poccup[match(maleSingDisp$Terrs, dfTerr$terr)])
    maleSingDisp$Pfledge <- as.numeric(dfTerr$Pfledge[match(maleSingDisp$Terrs, dfTerr$terr)])
    if (nrow(maleSingDisp[which(maleSingDisp$Terrs %in% sfTerrs$terr), ]) > 0) {
      females <- BTYdf[BTYdf$Sex == "F", ]
      maleSingDisp[which(maleSingDisp$Terrs %in% females$Terrs), ]$Mated <- 1
      nonDispFemales <- BTYdf[BTYdf$Sex == "F" & BTYdf$disp == 0, ]
      BTYdf[BTYdf$Sex == "F" & BTYdf$disp == 0 & BTYdf$Mated == 0, ]$Mated[which(sfTerrs$terr %in% maleSingDisp$Terrs)] <- 1
    }
    BTYdf <- rbind(BTYdf, maleSingDisp)
  }
  if(nrow(femaleSingDisp) > 0) {
    BTYdf <- na.omit(BTYdf)
    emptyTerrs <- dfTerr[which(!dfTerr$terr %in% BTYdf$Terrs), ]
    smTerrs <- dfTerr[which(dfTerr$terr %in% BTYdf[BTYdf$Sex == "M" & BTYdf$Mated == 0, ]$Terrs), ]
    sMales <- BTYdf[BTYdf$Sex == "M" & BTYdf$Mated == 0, ]
    nsMales <- nrow(sMales)
    ndsFemales <- nrow(femaleSingDisp)
    nPairable <- min(nsMales, ndsFemales)
    if (nPairable > 0) {
      mateDraw <- rbinom(nPairable, 1, pMate)
      
      sMales$Mated[1:nPairable] <- mateDraw
      femaleSingDisp$Mated[1:nPairable] <- mateDraw
      
      mateFemales <- femaleSingDisp[femaleSingDisp$Mated == 1, , drop = FALSE]
      unmateFemales <- femaleSingDisp[femaleSingDisp$Mated == 0, , drop = FALSE]
      mateMales <- sMales[sMales$Mated == 1, , drop = FALSE]
      # Assign mated females to male territories (no replacement)
      if (nrow(mateFemales) > 1) {
        mateFemales$Terrs <- as.numeric(sample(mateMales$Terrs,
                                               size = nrow(mateFemales),
                                               prob = as.numeric(mateMales$Poccup),
                                               replace = FALSE))
        mateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(mateFemales$Terrs, dfTerr$terr)])
        mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(mateFemales$Terrs, dfTerr$terr)])
      } # Closes if statement where we can use sample
      else {
        mateFemales$Terrs <- mateMales$Terrs
        mateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(mateFemales$Terrs, dfTerr$terr)])
        mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(mateFemales$Terrs, dfTerr$terr)])
      }
    }
    else {
      femaleSingDisp$Mated <- 0
      mateFemales <- femaleSingDisp[femaleSingDisp$Mated == 1, , drop = FALSE]
      unmateFemales <- femaleSingDisp[femaleSingDisp$Mated == 0, , drop = FALSE]
    }
    emptyTerrs <- dfTerr[which(!dfTerr$terr %in% BTYdf$Terrs), ]
    # Handle unmated females
    if (nrow(unmateFemales) > nrow(emptyTerrs)) {
      # If the logical statement above is satisfied. This means every territory is occupied.
      # if you introduce more birds next year than those that pass away after year 1, 
      # the simulation will break. It is recommended that you don't setup a simulation
      # where we have to consider this case.
      
      warning("More unmated females than empty territories. Sending extras to mate.")
      
      # Split females
      nEmpty <- nrow(emptyTerrs)
      # This will give a nonempty df if nEmpty is 0. We fix this near the bottom
      newUnmateFemales <- unmateFemales[1:nEmpty, , drop = FALSE]
      
      if (nEmpty < nrow(unmateFemales)) {
        newMateFemales <- unmateFemales[(nEmpty + 1):nrow(unmateFemales), , drop = FALSE]
      } else {
        newMateFemales <- unmateFemales[0, , drop = FALSE]
      }
      
      # Assign extra females to single males
      if (nrow(newMateFemales) > 0) {
        lonelyMaleTerrs <- as.numeric(sMales$Terrs[!(sMales$Terrs %in% mateFemales$Terrs)])
        lonelyPocc <- as.numeric(sMales$Poccup[!(sMales$Terrs %in% mateFemales$Terrs)])
        # If we need more females to mate but all males have a mate, then we run this error. 
        # I don't think we need this, but it's an extra safeguard
        if (length(lonelyMaleTerrs) == 0) {
          stop("All males have mated, but we still need a female to mate.")
        } # closes if statement for the error
        # Can't use sample if there is only one observation
        if (length(lonelyMaleTerrs) == 1) {
          newMateFemales$Mated <- 1
          newMateFemales$Terrs <- as.numeric(lonelyMaleTerrs)
          
          newMateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(newMateFemales$Terrs, dfTerr$terr)])
          newMateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(newMateFemales$Terrs, dfTerr$terr)])
        } # Closes if statement when there is only one single male
        else {
          newMateFemales$Mated <- 1
          newMateFemales$Terrs <- as.numeric(sample(lonelyMaleTerrs,
                                                    size = nrow(newMateFemales),
                                                    prob = lonelyPocc,
                                                    replace = FALSE))
          
          newMateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(newMateFemales$Terrs, dfTerr$terr)])
          newMateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(newMateFemales$Terrs, dfTerr$terr)])
        } # closes the case when there are multiple single males
        
        mateFemales <- rbind(mateFemales, newMateFemales)
      }
      
      # Assign remaining unmated females to empty territories
      if (nrow(newUnmateFemales) > 0 && nrow(emptyTerrs) > 0) {
        newUnmateFemales$Terrs <- as.numeric(sample(emptyTerrs$terr,
                                                    size = nrow(newUnmateFemales),
                                                    prob = as.numeric(emptyTerrs$Poccup),
                                                    replace = FALSE))
        newUnmateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(newUnmateFemales$Terrs, dfTerr$terr)])
        newUnmateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(newUnmateFemales$Terrs, dfTerr$terr)])
      }# closes statement that assigns single females territories to be alone.
      # want to make sure there are no single females if nEmpty is 0
      # nEmpty is the number of territories unoccupied by a male.
      if (nEmpty == 0) {
        newUnmateFemales <- mateFemales[0, , drop = FALSE]
      } # closes if statement for when nEmpty is 0
      finalFemales <- rbind(mateFemales, newUnmateFemales)
      BTYdf <- rbind(BTYdf, finalFemales)
      BTYdf[BTYdf$Sex == "M",]$Mated[BTYdf[BTYdf$Sex == "M",]$Terrs %in% finalFemales$Terrs[finalFemales$Mated == 1]] <- 1
    } else {
      # You want the simulation to run the code below. This is the case where
      # there are enough territories for all of the single and mated birds
      if (nrow(unmateFemales) > 0 && nrow(emptyTerrs) > 0) {
        unmateFemales$Terrs <- as.numeric(sample(emptyTerrs$terr,
                                                 size = nrow(unmateFemales),
                                                 prob = emptyTerrs$Poccup,
                                                 replace = FALSE))
        unmateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(unmateFemales$Terrs, dfTerr$terr)])
        unmateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(unmateFemales$Terrs, dfTerr$terr)])
      } # closes if statement that assigns single females to terrs when both are available
      finalFemales <- rbind(mateFemales, unmateFemales)
      BTYdf <- rbind(BTYdf, finalFemales)
      BTYdf[BTYdf$Sex == "M",]$Mated[which(BTYdf[BTYdf$Sex == "M",]$Terrs %in% finalFemales$Terrs[finalFemales$Mated == 1])] <- 1
    }
  }
  BTYdf <- BTYdf[, -c(9, 10)]
  fledge_year <- ((1/4) * sin((pi/2) * (year + 1))) + (3/4)
  BTYdf$Pfledge <- fledge_year * as.numeric(BTYdf$Pfledge)
  return(BTYdf)
}
set.seed(2187)
DF <- newMate(BTYdf, birds, territories, pFidel = 0.6, year = 1, pMate = 0.9)
