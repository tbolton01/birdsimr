# I wrap as.numeric around all vectors since they're initially strings in the input dfs. 
# This is probably overkill, but better safe than sorry.
initializeBirdsOnTerr <- function(dfTerr, dfBird, pMate){
  
  males <- dfBird[dfBird$Sex == "M" & dfBird$Yr == 1, ]
  females <- dfBird[dfBird$Sex == "F" & dfBird$Yr == 1, ]
  
  nMale <- nrow(males)
  nFemale <- nrow(females)
  nTerr <- nrow(dfTerr)
  
  if (nFemale > nTerr | nMale > nTerr) {
    stop("Cannot have more birds than territories")
  }
  
  # Assign males to territories
  males$Terrs <- as.numeric(sample(dfTerr$terr, size = nMale, prob = dfTerr$Poccup))
  males$Poccup <- as.numeric(dfTerr$Poccup[match(males$Terrs, dfTerr$terr)])
  males$Pfledge <- as.numeric(dfTerr$Pfledge[match(males$Terrs, dfTerr$terr)])
  
  # Shuffle females
  females <- females[sample(nrow(females)), ]
  
  # Assign mating status
  nPairable <- min(nMale, nFemale)
  # Looking at the min above allows us not to deal with all of the nested if statements above
  mateDraw <- rbinom(nPairable, 1, pMate)
  
  males$Mated <- 0
  females$Mated <- 0
  
  males$Mated[1:nPairable] <- mateDraw
  females$Mated[1:nPairable] <- mateDraw
  
  mateFemales <- females[females$Mated == 1, , drop = FALSE]
  unmateFemales <- females[females$Mated == 0, , drop = FALSE]
  mateMales <- males[males$Mated == 1, , drop = FALSE]
  # Assign mated females to male territories (no replacement)
  if (nrow(mateFemales) > 1) {
    mateFemales$Terrs <- as.numeric(sample(mateMales$Terrs,
                                           size = nrow(mateFemales),
                                           prob = mateMales$Poccup,
                                           replace = FALSE))
    mateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(mateFemales$Terrs, dfTerr$terr)])
    mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(mateFemales$Terrs, dfTerr$terr)])
  } # Closes if statement where we can use sample
  else {
    mateFemales$Terrs <- mateMales$Terrs
    mateFemales$Poccup <- as.numeric(dfTerr$Poccup[match(mateFemales$Terrs, dfTerr$terr)])
    mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[match(mateFemales$Terrs, dfTerr$terr)])
  } # Closes else statement where we cant use sample
  
  # Find territories without males on them
  occupiedTerrs <- males$Terrs
  emptyTerrs <- dfTerr[!(dfTerr$terr %in% occupiedTerrs), , drop = FALSE]
  
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
      print("Paul")
      lonelyMaleTerrs <- as.numeric(males$Terrs[!(males$Terrs %in% mateFemales$Terrs)])
      lonelyPocc <- as.numeric(males$Poccup[!(males$Terrs %in% mateFemales$Terrs)])
      print(lonelyMaleTerrs)
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
  }
  
  # Update male mating status based on final assignments
  males$Mated[males$Terrs %in% finalFemales$Terrs[finalFemales$Mated == 1]] <- 1
  
  males <- males[, c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
  
  df <- rbind(males, finalFemales)
  
  return(df)
}
# My goal is to rbind a male df, a mated female df, and an unmated female df
# The columns will then be Terr (This is the territory that a bird is occupying), 
# bird ID, sex, yr, p(occ), p(fledge), a cloumn of 0s and 1s that determine if the 
# bird has a mate, and maybe a column that gives the number of 
# fledge for this first year for pairs. Could also be open to having a separate 
# function that gives the fledge column. Maybe called something like "makeBabies"

#territories <- createTerr(4)
#birds <- createBirds(6, Nyr = 1, maleRatio = 0.6, propNew = 1)
initializeBirdsOnTerr(territories, birds, 0.6)

