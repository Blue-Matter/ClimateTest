
# === Make single fleet, single sex OMs for climate tests ======================

# Tom Carruthers & Quang Huynh
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
nsim <- 12

### BUM
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2018 BUM SS3\\SS_BASE_2018_v3"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BUM.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_BUM.rds"))

### WHM
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2019 WHM SS3\\StockSynthesis\\Model_6"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_WHM.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_WHM.rds"))

### SMA
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2019 SMA SS3\\run_1_try_09_0"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_SMA.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_SMA.rds"))

### BSH
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\BSH\\Preliminary_Run_6_input"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
MOM@cpars$Fec_age <- 39 * MOM@cpars$Mat_age
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BSH.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_BSH.rds"))

### SWO
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\ICCAT_SWO_Assessment\\NSWO_MSE_SS3_Base_v2"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_SWO.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_SWO.rds"))

### BET
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2021 BET\\M20_h0.8_sigmaR0.4"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
Hist <- runMSE(OM, Hist = TRUE, checkMPs = FALSE, parallel = FALSE)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BET.rds"))
saveRDS(Hist, file = paste0(CTdir,"/OMs/Hist_BET.rds"))


# === End of script ===========================================================



