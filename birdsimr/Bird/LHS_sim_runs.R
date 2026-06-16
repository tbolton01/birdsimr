library(lhs)
source("birdSim.R")
set.seed(2187)
p_mat <- randomLHS(20, 4)
colnames(p_mat) <- c("pFidel", "pDispP", "pDispM", "pDispF")
write.csv(p_mat, "probs.csv", row.names = FALSE)
propNewVec <- c(rep(0.1, 5), rep(0.15, 5), rep(0.2, 10))

for(i in 1:nrow(p_mat)) {
  dat <- birdSim(Nbird = 500, maleRatio = 0.5, Nyr = 20, avgLifespan = 3, propNew = propNewVec,
                 Nterr = 300,
                 pFidel = p_mat[i, 1], pDispP = p_mat[i, 2], pDispM = p_mat[i, 3], pDispF = p_mat[i, 4],
                 maxFledge = 4,
                 pObsM = 0.65, pObsF = 0.95)
  write.csv(dat, paste0("sim_", i, ".csv"), row.names = FALSE)
}