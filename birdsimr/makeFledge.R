# This function takes in as inpute a df where the birds are on territories for a 
# specific year. Max fledge is the largest number of fledge you would see in 
# one year. 
makeFledge <- function(BTYdf, maxFledge) {
  matedMales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "M", ]
  terrsWMate <- as.numeric(matedMales$Terrs)
  pFledge <- as.numeric(matedMales$Pfledge)
  matedMales$Fledge <- rbinom(length(terrsWMate), 4, pFledge)
  matedFemales <- BTYdf[BTYdf$Mated == 1 & BTYdf$Sex == "F", ]
  matedFemales$Fledge <- as.numeric(matedMales$Fledge[match(matedFemales$Terrs, matedMales$Terrs)])
  unmated <- BTYdf[BTYdf$Mated == 0, ]
  unmated$Fledge <- rep(0, length(unmated$Mated))
  dfNew <- rbind(matedMales, matedFemales, unmated)
  return(dfNew)
}
makeFledge(BTYdf, 4)

