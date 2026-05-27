# I wrap as.numeric around all vectors since they're initially strings in the input dfs. 
# This is probably overkill, but better safe than sorry.
initializeBirdsOnTerr <- function(dfTerr, dfBird, year){
  fledge_year <- ((1/4) * sin(pi * year)) + (3/4)
  dfBird$pFledge <- fledge_year * as.numeric(dfBird$pFledge)
  males <- dfBird[dfBird$Sex == "M" & dfBird$Yr == (year), ]
  females <- dfBird[dfBird$Sex == "F" & dfBird$Yr == (year), ]
  nMale <- nrow(males)
  nFemale <- nrow(females)
  nTerr <- nrow(dfTerr)
  # Conditionals to make sure that we have birds and territories so that the 
  # simulation can go on.
  if (nMale == 0 & nFemale == 0) {
    stop("No birds to place on territories")
  }
  if (nTerr == 0) {
    stop("No territories to place birds on.")
  }
  if (nFemale > nTerr | nMale > nTerr) {
    stop("Cannot have more birds than territories")
  }
  if (nMale > 0 & nFemale == 0) {
    males$Terrs <- as.numeric(sample(dfTerr$terr, size = nMale))
    males$Mated <- rep(0, nMale)
    males <- males[, c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "pMate", "pFledge")]
    return(males)
  }
  if (nMale == 0 & nFemale > 0) {
    females$Terrs <- as.numeric(sample(dfTerr$terr, size = nFemale))
    females$Mated <- rep(0, nFemale)
    females <- females[, c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "pMate", "pFledge")]
    return(females)
  }
  # Assign males to territories
  males$Terrs <- as.numeric(sample(dfTerr$terr, size = nMale))
  
  # Shuffle males
  males <- males[sample(nrow(males)), ]
  
  # Assign mating status
  nPairable <- min(nMale, nFemale)
  # Looking at the min above allows us not to deal with all of the nested if statements above
  malesMate <- rep(0, nrow(males))
  malesMate <- rbinom(nMale, 1, as.numeric(males$pMate))
  males$Mated <- rep(0, nMale)
  females$Mated <- rep(0, nFemale)
  
  # All of this sibsetting of 1:nPariable makes sure we don't have 
  # more mated males than there are females
  males$Mated[1:nPairable] <- malesMate[1:nPairable]
  females$Mated[1:nPairable] <- malesMate[1:nPairable]
  mateFemales <- females[females$Mated == 1, , drop = FALSE]
  unmateFemales <- females[females$Mated == 0, , drop = FALSE]
  mateMales <- males[males$Mated == 1, , drop = FALSE]
  # Assign mated females to male territories (no replacement)
  if (nrow(mateFemales) > 1) {
    mateFemales$Terrs <- as.numeric(sample(mateMales$Terrs,
                                           size = nrow(mateFemales),
                                           replace = FALSE))
  } # Closes if statement where we can use sample
  else {
    mateFemales$Terrs <- mateMales$Terrs
  } # Closes else statement where we cant use sample
  
  # Find territories without males on them
  occupiedTerrs <- males$Terrs
  emptyTerrs <- dfTerr[!(dfTerr$terr %in% occupiedTerrs), , drop = FALSE]
  # Handle unmated females
  print("John")
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
      lonelyMaleTerrs <- as.numeric(males$Terrs[!(males$Terrs %in% mateFemales$Terrs)])
      # If we need more females to mate but all males have a mate, then we run this error. 
      # I don't think we need this, but it's an extra safeguard
      if (length(lonelyMaleTerrs) == 0) {
        stop("All males have mated, but we still need a female to mate.")
      } # closes if statement for the error
      # Can't use sample if there is only one observation
      if (length(lonelyMaleTerrs) == 1) {
        newMateFemales$Mated <- 1
        newMateFemales$Terrs <- as.numeric(lonelyMaleTerrs)
      } # Closes if statement when there is only one single male
      else {
        newMateFemales$Mated <- 1
        newMateFemales$Terrs <- as.numeric(sample(lonelyMaleTerrs,
                                                  size = nrow(newMateFemales),
                                                  replace = FALSE))
      } # closes the case when there are multiple single males
      
      mateFemales <- rbind(mateFemales, newMateFemales)
    }
    
    # Assign remaining unmated females to empty territories
    if (nrow(newUnmateFemales) > 0 && nrow(emptyTerrs) > 0) {
      
      newUnmateFemales$Terrs <- as.numeric(sample(emptyTerrs$terr,
                                                  size = nrow(newUnmateFemales),
                                                  replace = FALSE))
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
                                               replace = FALSE))
    } # closes if statement that assigns single females to terrs when both are available
    finalFemales <- rbind(mateFemales, unmateFemales)
  }
  
  # Update male mating status based on final assignments
  males$Mated[males$Terrs %in% finalFemales$Terrs[finalFemales$Mated == 1]] <- 1
  males <- males[, c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "pMate", "pFledge")]
  finalFemales <- finalFemales[, c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "pMate", "pFledge")]
  
  df <- rbind(males, finalFemales)
  
  return(df)
}
# My goal is to rbind a male df, a mated female df, and an unmated female df
# The columns will then be Terr (This is the territory that a bird is occupying), 
# bird ID, sex, yr, p(occ), p(fledge), a column of 0s and 1s that determine if the 
# bird has a mate, and maybe a column that gives the number of 
# fledge for this first year for pairs. Could also be open to having a separate 
# function that gives the fledge column. Maybe called something like "makeBabies"

territories <- createTerr(5)
#birds <- createBirds(8, Nyr = 1, maleRatio = 0.6, propNew = 1)
initializeBirdsOnTerr(territories, birds, year = 1)

