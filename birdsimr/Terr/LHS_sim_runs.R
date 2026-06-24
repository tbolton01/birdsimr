library(lhs)
source("simTerr.R")
set.seed(2187)
p_mat <- randomLHS(20, 4)
#territory effect should clearly be present on this run
p_high <- c(0.1, 0.90, 0.95, 0.95)
#territory effect should be a bit more unclear on this run
p_low <- c(0.97, 0.005, 0.05, 0.01)
p_mat <- rbind(p_mat, p_high, p_low)
colnames(p_mat) <- c("pFidel", "pDispP", "pDispM", "pDispF")
write.csv(p_mat, "probs.csv", row.names = FALSE)
propNewVec <- c(rep(0.1, 5), rep(0.15, 5), rep(0.2, 10))

for(i in 1:nrow(p_mat)) {
  dat <- terrSim(Nbird = 250, maleRatio = 0.5, Nyr = 20, avgLifespan = 3, propNew = propNewVec,
                 Nterr = 120, probQual = c(0.1, 0.3, 0.6), aBad = 5, bBad = (5/0.45) - 5, 
                 aMid = 9, bMid = (9/0.55) - 9, aGood = 14, bGood = (14/0.65) - 14, pMate = 0.9, 
                 pFidel = p_mat[1, 1], pDispP = p_mat[1, 2], pDispM = p_mat[1, 3], pDispF = p_mat[1, 4],
                 maxFledge = 4,
                 pObsM = 0.65, pObsF = 0.95)
  write.csv(dat, paste0("sim_", i, ".csv"), row.names = FALSE)
}