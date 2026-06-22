library(lhs)
source("simTerr.R")
set.seed(2187)
p_mat <- randomLHS(20, 4)
colnames(p_mat) <- c("pFidel", "pDispP", "pDispM", "pDispF")
write.csv(p_mat, "probs.csv", row.names = FALSE)
propNewVec <- c(rep(0.1, 5), rep(0.15, 5), rep(0.2, 10))

for(i in 1:nrow(p_mat)) {
  dat <- terrSim(Nbird = 500, maleRatio = 0.5, Nyr = 20, avgLifespan = 3, propNew = propNewVec,
                 Nterr = 300, probQual = c(0.1, 0.3, 0.6), aBad = 1, bBad = 3, 
                 aMid = 1, bMid = 1, aGood = 2, bGood = 1, pMate = 0.9, 
                 pFidel = p_mat[1, 1], pDispP = p_mat[1, 2], pDispM = p_mat[1, 3], pDispF = p_mat[1, 4],
                 maxFledge = 4,
                 pObsM = 0.65, pObsF = 0.95)
  write.csv(dat, paste0("sim_", i, ".csv"), row.names = FALSE)
}