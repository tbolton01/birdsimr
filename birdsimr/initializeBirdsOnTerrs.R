initializeBirdsOnTerr <- function(dfTerr, dfBird, pMate){
  # Since we are initializing the territories with the first round of birds,
  # we only want to include birds that were spotted in year 1.
  males <- dfBird[dfBird$Sex == "M" & dfBird$Yr == 1]
  females <- dfBird[dfBird$Sex == "F" & dfBird$Yr == 1]
  nMale <- length(males$Sex)
  nFemale <- length(females$Sex)
  nTerr <- length(dfTerr$terr)
  # Make sure every bird has a territory to go to
  if (nFemale > nTerr | nMale > nTerr) {
    stop("Cannot have more male birds or female birds than number of territories")
  }
  males$Terrs <- sample(dfTerr$terr, size = nMale, prob = dfTerr$Poccup)
  males$Poccup <- dfTerr$Poccup[males$Terrs]
  males$Pfledge <- dfTerr$fledge[males$Terrs]
  females <- shuffle(females)
  if (nFemale > nMale){
    # We cannot draw nFemale random bernoulli numbers otherwise we might set
    # some females up to have a mate, but have no male counterpart available
    males$Mated <- rbinom(nMale, 1, pMate)
    females$Mated <- c(males$Mated, rep(0, nFemale - length(mateFemales$Sex)))
    mateFemales <- females[which(females$Mated == 1), ]
    # If the logical below isn't met, this will be a vector of of 0s and 1s that will
    # determine if a female is mated
    unmateFemales <- females[which(females$Mated == 0), ]
    mateFemales$Terrs <- sample(males$Terrs, size = length(mateFemales), prob = males$Poccup)
    emptyTerrs <- dfTerr[-which(dfTerr$terr %in% males$Terrs),]
    if (length(unmateFemales$Sex) > length(emptyTerrs$terr)) {
      # You start to get into dicey territory if the logical statement above is 
      # satisfied. If the logical statement above is satisfied. This means every territory is occupied.
      # if you introduce more birds next year than those that pass away after year 1, 
      # the simulation will break. It is recommended that you don't setup a simulation
      # where we have to consider this case.
      warning("More remaining females than empty territories. Some more females are being sent to mate.")
      newUnmateFemales <- unmateFemales[1:length(emptyTerrs$terr), ]
      # These females will be sent to male occupied territories that don't yet 
      # have a female
      newMateFemales <- unmateFemales[(length(emptyTerrs$terr) + 1):length(unmateFemales$Sex), ]
      lonelyMaleTerrs <- males$Terrs[-which(male$Terrs %in% mateFemales$Terrs)]
      lonelyPocc <- males$Poccup[-which(male$Terrs %in% mateFemales$Terrs)]
      newMateFemales$Mated <- rep(1, length(newMateFemales$Mated))
      newMateFemales$Terrs <- sample(lonelyMaleTerrs, size = length(newMateFemales$Mated, prob = lonelyPocc))
      mateFemales <- rbind(mateFemales, newMateFemales)
      mateFemales$Poccup <- dfTerr$Poccup[mateFemales$Terrs]
      mateFemales$Pfledge <- dfTerr$Pfledge[mateFemales$Terrs]
      newUnmateFemales$Terrs <- sample(emptyTerrs$terr, length(emptyTerrs$terr), prob = emptyTerrs$Poccup)
      newUnmateFemales$Poccup <- dfTerr$Poccup[newUnmateFemales$Terrs]
      newUnmateFemales$Pfledge <- dfTerr$Pfledge[newUnmateFemales$Terrs]
      df <- rbind(males, mateFemales, newUnmateFemales)
      # will want to add nice column names. However, I think it will be best to see the output before adding this
      return(df)
    }# close if statement that will result in all territories being occupied
    else {
      # Since there are more territories than unmated females, we're just sending the unmated females
      # off to empty territories. There will still be some empty territories unless equality is held in the previous
      # logical statement
      UnmateFemales$Terrs <- sample(emptyTerrs$terr, length(UnmateFemales$Sex), prob = emptyTerrs$Poccup)
      newUnmateFemales$Poccup <- dfTerr$Poccup[UnmateFemales$Terrs]
      newUnmateFemales$Pfledge <- dfTerr$Pfledge[UnmateFemales$Terrs]
      df <- rbind(males, mateFemales, newUnmateFemales)
      # will want to add nice column names. However, I think it will be best to see the output before adding this
      return(df)
    }# Close else statement which accounts for the case when n empty terrs <= number of single females
  } # closes if statement which accounts for the case when n female > n male
  # Need to do the same assignment procedure in the case when n female <= n male
  # Do with else statement
  # should be similar to the above
}
# My goal is to rbind a male df, a mated female df, and an unmated female df
# The columns will then be Terr (This is the territory that a bird is occupying), 
# bird ID, sex, yr, p(occ), p(fledge), a cloumn of 0s and 1s that determine if the 
# bird has a mate, and maybe a column that gives the number of 
# fledge for this first year for pairs. Could also be open to having a separate 
# function that gives the fledge column. Maybe called something like "makeBabies"