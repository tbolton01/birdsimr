initializeBirdsOnTerr <- function(dfTerr, dfBird, pMate){
  # Since we are initializing the territories with the first round of birds,
  # we only want to include birds that were spotted in year 1.
  males <- dfBird[dfBird$Sex == "M" & dfBird$Yr == 1, ]
  females <- dfBird[dfBird$Sex == "F" & dfBird$Yr == 1, ]
  nMale <- length(males$Sex)
  nFemale <- length(females$Sex)
  nTerr <- length(dfTerr$terr)
  # Make sure every bird has a territory to go to
  if (nFemale > nTerr | nMale > nTerr) {
    stop("Cannot have more male birds or female birds than number of territories")
  }
  males$Terrs <- as.numeric(sample(dfTerr$terr, size = nMale, prob = dfTerr$Poccup))
  # Territory IDs are strings, so we convert to numeric to use for indexing
  males$Poccup <- as.numeric(dfTerr$Poccup[as.numeric(males$Terrs)])
  males$Pfledge <- as.numeric(dfTerr$Pfledge[as.numeric(males$Terrs)])
  females <- females[sample(nrow(females)), ]
  if (nFemale > nMale){
    print("Darth Vader")
    # We cannot draw nFemale random bernoulli numbers otherwise we might set
    # some females up to have a mate, but have no male counterpart available
    males$Mated <- rbinom(nMale, 1, pMate)
    females$Mated <- c(males$Mated, rep(0, nFemale - nMale))
    mateFemales <- females[which(females$Mated == 1), ]
    # If the logical below isn't met, this will be a vector of of 0s and 1s that will
    # determine if a female is mated
    unmateFemales <- females[which(females$Mated == 0), ]
    mateFemales$Terrs <- sample(males$Terrs, size = length(mateFemales$Mated), prob = males$Poccup)
    mateFemales$Poccup <- as.numeric(dfTerr$Poccup[as.numeric(mateFemales$Terrs)])
    mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[as.numeric(mateFemales$Terrs)])
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
      if (length(which(as.numeric(mateFemales$Terrs) %in% as.numeric(males$Terrs))) == 0) {
        lonelyMaleTerrs <- as.numeric(males$Terrs)
        lonelyPocc <- males$Poccup
      } # handles the exception where there are no mates
      else {
        print("Rey")
        lonelyMaleTerrs <- as.numeric(males$Terrs[-which(as.numeric(mateFemales$Terrs) %in% as.numeric(males$Terrs))])
        print(lonelyMaleTerrs)
        lonelyPocc <- as.numeric(males$Poccup[-which(males$Terrs %in% mateFemales$Terrs)])
      } # handles the case where there is at least one mated pair before having to assign more females to mate
      newMateFemales$Mated <- rep(1, length(newMateFemales$Mated))
      if (length(lonelyMaleTerrs) == 1 ) {
        # we have this because if we only have one terr, the sample function won't work and we'll
        # just send the female that needs to mate to his terr
        print("beckett")
        newMateFemales$Terrs <- lonelyMaleTerrs
        print(newMateFemales)
        print(mateFemales)
      }# closes if statement for when there is only 1 single male that needs a mate
      else {
        newMateFemales$Terrs <- sample(lonelyMaleTerrs, size = length(newMateFemales$Mated), prob = lonelyPocc)
      }# closes case when there is more than one male that needs to go and mate
      newMateFemales$Poccup <- dfTerr$Poccup[newMateFemales$Terrs]
      newMateFemales$Pfledge <- dfTerr$Pfledge[newMateFemales$Terrs]
      mateFemales <- rbind(mateFemales, newMateFemales)
      print("Lawson")
      if (length(emptyTerrs$terr) == 1) {
        newUnmateFemales$Terrs <- emptyTerrs$terr
        print("Hux")
      } # handling the case where there is only 1 empty territory and we can't use
      # the sample function
      else {
        newUnmateFemales$Terrs <- as.numeric(sample(emptyTerrs$terr, length(emptyTerrs$terr), prob = emptyTerrs$Poccup))
        } #closes else statment creating unmated females terrs
      newUnmateFemales$Poccup <- dfTerr$Poccup[as.numeric(newUnmateFemales$Terrs)]
      newUnmateFemales$Pfledge <- dfTerr$Pfledge[as.numeric(newUnmateFemales$Terrs)]
      males$Mated[which(males$Terrs %in% mateFemales$Terrs)] <- 1
      males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
      df <- rbind(males, mateFemales, newUnmateFemales)
      print("Snoke")
      return(df)
    }# close if statement that will result in all territories being occupied
    else {
      # Since there are more territories than unmated females, we're just sending the unmated females
      # off to empty territories. There will still be some empty territories unless equality is held in the previous
      # logical statement
      unmateFemales$Terrs <- sample(emptyTerrs$terr, length(unmateFemales$Sex), prob = emptyTerrs$Poccup)
      unmateFemales$Poccup <- as.numeric(dfTerr$Poccup[as.numeric(unmateFemales$Terrs)])
      unmateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[as.numeric(unmateFemales$Terrs)])
      print("Chewie")
      males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
      df <- rbind(males, mateFemales, unmateFemales)
      return(df)
    }# Close else statement which accounts for the case when n empty terrs >= number of single females
  } # closes if statement which accounts for the case when n female > n male
  else {
    print("Luke Skywalker")
    females$Mated <- rbinom(nFemale, 1, pMate)
    males$Mated <- c(females$Mated, rep(0, nMale - nFemale))
    mateFemales <- females[which(females$Mated == 1), ]
    unmateFemales <- females[which(females$Mated == 0), ]
    mateFemales$Terrs <- sample(males$Terrs, size = length(mateFemales$Mated), prob = males$Poccup)
    print(mateFemales)
    mateFemales$Poccup <- as.numeric(dfTerr$Poccup[as.numeric(mateFemales$Terrs)])
    mateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[as.numeric(mateFemales$Terrs)])
    emptyTerrs <- dfTerr[-which(dfTerr$terr %in% males$Terrs),]
    # Once again, we need to make sure each unmated female has an empty territory to go to
    # if not, they're going off to mate 
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
      if (length(which(as.numeric(mateFemales$Terrs) %in% as.numeric(males$Terrs))) == 0) {
        lonelyMaleTerrs <- as.numeric(males$Terrs)
        lonelyPocc <- males$Poccup
      } # handles the exception where there are no mates
      else {
        print("Rey")
        print(which(mateFemales$Terrs %in% males$Terrs))
        # There is a bug in the line below I need to diagnose
        lonelyMaleTerrs <- as.numeric(males$Terrs[-which(mateFemales$Terrs %in% males$Terrs)])
        print(lonelyMaleTerrs)
        lonelyPocc <- as.numeric(males$Poccup[-which(males$Terrs %in% mateFemales$Terrs)])
      } # handles the case where there is at least one mated pair before having to assign more females to mate
      newMateFemales$Mated <- rep(1, length(newMateFemales$Mated))
      if (length(lonelyMaleTerrs) == 1 ) {
        # we have this because if we only have one terr, the sample function won't work and we'll
        # just send the female that needs to mate to his terr
        print("beckett")
        newMateFemales$Terrs <- lonelyMaleTerrs
        print(newMateFemales)
        print(mateFemales)
      }# closes if statement for when there is only 1 single male that needs a mate
      else {
        print(length(newMateFemales$Mated))
        print(lonelyPocc)
        print(lonelyMaleTerrs)
        newMateFemales$Terrs <- sample(lonelyMaleTerrs, size = length(newMateFemales$Mated), prob = lonelyPocc)
        print("aura")
        print(newMateFemales)
      }# closes case when there is more than one male that needs to go and mate
      newMateFemales$Poccup <- dfTerr$Poccup[newMateFemales$Terrs]
      newMateFemales$Pfledge <- dfTerr$Pfledge[newMateFemales$Terrs]
      mateFemales <- rbind(mateFemales, newMateFemales)
      print(mateFemales)
      print("Lawson")
      if (length(emptyTerrs$terr) == 0) {
        males$Mated[which(males$Terrs %in% mateFemales$Terrs)] <- 1
        males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
        print("Krennic")
        df <- rbind(males, mateFemales)
        return(df)
      }
      if (length(emptyTerrs$terr) == 1) {
        newUnmateFemales$Terrs <- emptyTerrs$terr
        print("Hux")
      } # handling the case where there is only 1 empty territory and we can't use
      # the sample function
      else {
        print("Meero")
        print(emptyTerrs)
        newUnmateFemales$Terrs <- as.numeric(sample(emptyTerrs$terr, length(emptyTerrs$terr), prob = emptyTerrs$Poccup))
        print("Han Solo")
      } #closes else statment creating unmated females terrs
      newUnmateFemales$Poccup <- dfTerr$Poccup[as.numeric(newUnmateFemales$Terrs)]
      newUnmateFemales$Pfledge <- dfTerr$Pfledge[as.numeric(newUnmateFemales$Terrs)]
      males$Mated[which(mateFemales$Terrs %in% males$Terrs)] <- 1
      males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
      df <- rbind(males, mateFemales, newUnmateFemales)
      print("Snoke")
      return(df)
    }# close if statement that will result in all territories being occupied
    else {
      # Since there are more territories than unmated females, we're just sending the unmated females
      # off to empty territories. There will still be some empty territories unless equality is held in the previous
      # logical statement
      print("Hello there")
      if (length(emptyTerrs$terr) == 0) {
        #handling the case where there are no unmated females so we can't create a df of them
        print("John Lennon")
        print(mateFemales$Terrs)
        print(males$Mated)
        print(males)
        males$Mated[which(mateFemales$Terrs %in% males$Terrs)] <- 1
        males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
        df <- rbind(males, mateFemales)
        return(df)
      }
      unmateFemales$Terrs <- sample(emptyTerrs$terr, length(unmateFemales$Sex), prob = emptyTerrs$Poccup)
      unmateFemales$Poccup <- as.numeric(dfTerr$Poccup[as.numeric(unmateFemales$Terrs)])
      unmateFemales$Pfledge <- as.numeric(dfTerr$Pfledge[as.numeric(unmateFemales$Terrs)])
      males <- males[c("birdID", "Sex", "Lifespan", "Yr", "Mated", "Terrs", "Poccup", "Pfledge")]
      df <- rbind(males, mateFemales, unmateFemales)
      # will want to add nice column names. However, I think it will be best to see the output before adding this
      return(df)
    } # closes the case when all initial single females can stay single
  } # closes else when n female <= n male
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

#territories <- createTerr(4)
#birds <- createBirds(6, Nyr = 1, maleRatio = 0.6, propNew = 1)
initializeBirdsOnTerr(territories, birds, 0.6)

