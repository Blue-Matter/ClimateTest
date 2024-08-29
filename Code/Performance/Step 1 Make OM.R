
# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 1 make operating model

library(openMSE)
setwd('C:/GitHub/ClimateTest')

OM0 = readRDS("OMs/Robustness_testing/OM_BET.rds")
OM = SubCpars(OM,1:36)
Hist0 = runMSE(OM, Hist=T)                                    # run a historical spool up to get reference points
Data = new('Data')
#OM = SS2OM('Assessment/Preliminary_Run_6_input',nsim=36)     # sample var-covar to make OpenMSE class OM
#Data = SS2Data('Assessment/Preliminary_Run_6_input')         # convert SS3 input data to OpenMSE class Data
#Data@CAL = array(NA,c(1,1,1))                                # don't simulate CAL data
Data@MPrec = sum(Hist0@TSdata$Landings[1,OM@nyears,])          # assume that the recent catch observation is the current TAC
OM@cpars$Data = Data                                         # add real data to OM 
OM@cpars$qs = NULL                                           # estimate catchability to match specified depletion
Dep_adjust = Hist@Ref$ReferencePoints$SSBMSY_SSB0[1]/OM@D[1]  # make starting depletion closer to mean = BMSY (SSB)
OM@cpars$D = trlnorm(OM@nsim, OM@D[1]*Dep_adjust,0.2)                  # distribution of current depletion
Hist = runMSE(OM, Hist=T)                                     # run a historical spool up to get reference points

saveRDS(OM,"OMs/Performance/BET.rds")
saveRDS(Hist,"OMs/Performance/Hist.rds")

OM_tune = SubCpars(OM,1:8)
Hist_tune = runMSE(OM_tune,Hist=T)
saveRDS(OM_tune,"OMs/Performance/BET_tune.rds")
saveRDS(Hist_tune,"OMs/Performance/Hist_tune.rds")




# =========== END OF SCRIPT ====================================================================================================

