# Could look at making propNew a vector of proportions of length Nyr
createBirds <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew){
  birdVec <- c(1:Nbird)
  #print(birdVec)
  lifespanVec <- rgeom(Nbird, 1/avgLifespan) + 1 
  # adding 1 because I want that to be the number of years we observe a bird
  # and I think if a bird is in this dataset, we should observe it at least
  # 1 year
  #print(lifespanVec)
  sexVec <- sample(c("M", "F"), Nbird, replace = TRUE, prob = c(maleRatio, 1 - maleRatio))
  sex <- c()
  birdy <- c()
  lifetime <- c()
  yr <- c()
  for (i in seq_len(Nyr)){
    if (floor(length(birdVec) * propNew) < 1) {
      break
    }
    print(i)
    birds <- sort(sample(birdVec, floor(length(birdVec) * propNew)))
    print(birds)
    spans <- lifespanVec[which(birdVec %in% birds)]
    print(spans)
    sexes <- sexVec[which(birdVec %in% birds)]
    for (j in seq_len(length(spans))) {
      # Nyr - year i should give us remaining years in the study
      # we need to add 1 here since we added 1 to the geometric values
      if((Nyr - i) + 1 - spans[j] >= 0) {
        lifetime <- c(lifetime, rep(spans[j], spans[j]))
        yr <- c(yr, i:(i + spans[j] - 1))
        # Subtracting that 1 is what allows us to only see a bird with a lifespan
        # of 1 in only 1 year
        birdy <- c(birdy, rep(birds[j], spans[j]))
        sex <- c(sex, rep(sexes[j], spans[j]))
      } # if statement
      else {
        # adding 1 to these quantities again to account for the + 1 on the
        # geometric output
        lifetime <- c(lifetime, rep(spans[j], Nyr - i + 1))
        yr <- c(yr, i:Nyr)
        birdy <- c(birdy, rep(birds[j], Nyr - i + 1))
        sex <- c(sex, rep(sexes[j], Nyr - i + 1))
      } # else
    } # for j
    lifespanVec <- lifespanVec[-which(birdVec %in% birds)]
    #print(which(birdVec %in% birds))
    sexVec <- sexVec[-which(birdVec %in% birds)]
    birdVec <- birdVec[-which(birdVec %in% birds)]
    #print(birdVec)
    #print(lifespanVec)
  }#for i
  df <- data.frame(cbind(birdy, sex, lifetime, yr))
  colnames(df) <- c("birdID", "Sex", "Lifespan", "Yr")
  return(df)
}