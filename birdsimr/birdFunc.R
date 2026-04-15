# Could look at making propNew a vector of proportions of length Nyr
createBirds <- function(Nbird, maleRatio = 0.5, Nyr, avgLifespan = 3, propNew){
  birdVec <- c(1:Nbird)
  lifespanVec <- rgeom(Nbird, 1/avgLifespan) + 1 
  # adding 1 because I want that to be the number of years we observe a bird
  # and I think if a bird is in this dataset, we should observe it at least
  # 1 year
  sexVec <- sample(c("M", "F"), Nbird, replace = TRUE, prob = c(maleRatio, 1 - maleRatio))
  # These vectors are used i the creation of the final df
  sex <- c()
  birdy <- c()
  lifetime <- c()
  yr <- c()
  for (i in seq_len(Nyr)){
    # if we observe 0 birds the spans vector will be empty and break the logical
    # statement that involves its jth component
    if (floor(length(birdVec) * propNew) < 1) {
      break
    }
    # here we sample and get a vector of birds we FIRST see in year i
    # if their lifespan is bigger than 1, we'll see them again next year as long
    # as i neq Nyr. However , the j loop runs out the rest of the years they are
    # alive
    birds <- sort(sample(birdVec, floor(length(birdVec) * propNew)))
    # Below are vectors with basic information corresponding to the birds
    # in the vector above
    spans <- lifespanVec[which(birdVec %in% birds)]
    sexes <- sexVec[which(birdVec %in% birds)]
    for (j in seq_len(length(spans))) {
      # Nyr - year i should give us remaining years in the study
      # we need to add 1 here since we added 1 to the geometric values
      if((Nyr - i) + 1 - spans[j] >= 0) {
        # the "if" statement above checks to see if the birds lifespan is shorter
        # than the remaining years for generating data
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
        # here we are looking at birds whose lifespans are longer than the 
        # remaining time after they were first observed in year i
        lifetime <- c(lifetime, rep(spans[j], Nyr - i + 1))
        # instead of repping everything by their lifespan, we rep it by remaining
        # time in the simulation since their lifespan is bigger than the number of
        # remaining years for which we are generating data
        yr <- c(yr, i:Nyr)
        birdy <- c(birdy, rep(birds[j], Nyr - i + 1))
        sex <- c(sex, rep(sexes[j], Nyr - i + 1))
      } # else
    } # for j
    # we remove birds and their characteristics because they were first observed 
    # in year i, so it wouldn't make sense to have them first seen in year and
    # then observed again for the first time in year i + 1
    lifespanVec <- lifespanVec[-which(birdVec %in% birds)]
    sexVec <- sexVec[-which(birdVec %in% birds)]
    birdVec <- birdVec[-which(birdVec %in% birds)]
  }#for i
  df <- data.frame(cbind(birdy, sex, lifetime, yr))
  colnames(df) <- c("birdID", "Sex", "Lifespan", "Yr")
  return(df)
}