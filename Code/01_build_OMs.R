
# === Make single fleet, single sex OMs for climate tests ======================

# Tom Carruthers & Quang Huynh
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
nsim <- 24

add_qs_cv_inc = function(OM,qcv = c(0.1,0.15),qinc=c(-0.005,0.005)){
  OM@qcv = qcv
  OM@qinc = qinc
  OM@interval=3
  OM
}

### BUM
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2018 BUM SS3\\SS_BASE_2018_v3"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BUM.rds"))

### WHM
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2019 WHM SS3\\StockSynthesis\\Model_6"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_WHM.rds"))

### SMA
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2019 SMA SS3\\run_1_try_09_0"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_SMA.rds"))

### BSH
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\BSH\\Preliminary_Run_6_input"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM@cpars$Fec_age <- 39 * OM@cpars$Mat_age
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BSH.rds"))

### SWO
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\ICCAT_SWO_Assessment\\NSWO_MSE_SS3_Base_v2"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_SWO.rds"))

### BET
ssdir <- "G:\\Shared drives\\BM shared\\1. Projects\\EcoTest\\Assessments\\2021 BET\\M20_h0.8_sigmaR0.4"
replist <- r4ss::SS_output(ssdir)
OM <- SS2OM(replist, nsim = nsim)
OM = add_qs_cv_inc(OM)
saveRDS(OM, file = paste0(CTdir,"/OMs/OM_BET.rds"))


# === End of script ===========================================================



