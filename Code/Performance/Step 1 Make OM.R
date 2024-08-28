
# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 1 make operating model

library(openMSE)
setwd('C:/GitHub/ClimateTest')

OM = SS2OM('Assessment/Preliminary_Run_6_input',nsim=36)     # sample var-covar to make OpenMSE class OM
Data = SS2Data('Assessment/Preliminary_Run_6_input')         # convert SS3 input data to OpenMSE class Data
Data@CAL = array(NA,c(1,1,1))                                # don't simulate CAL data
Data@MPrec = Data@Cat[1,ncol(Data@Cat)]                      # assume that the recent catch observation is the current TAC
OM@cpars$Data = Data                                         # add real data to OM 
OM@cpars$qs = NULL                                           # estimate catchability to match specified depletion
Dep_adjust = Hist@Ref$ReferencePoints$SSBMSY_SSB0[1]/OM@D[1] # make starting depletion closer to mean = BMSY (SSB)
OM@cpars$D = trlnorm(OM@nsim,OM@D[1]*Dep_adjust,0.2)         # distribution of current depletion
Hist = runMSE(OM, Hist=T)                                    # run a historical spool up to get reference points

saveRDS(OM,"OMs/Performance/BSH.rds")
saveRDS(Hist,"OMs/Performance/Hist.rds")

OM_tune_1 = SubCpars(OM,1:6)                                 # tuning OM is just 18 simulations
Hist_tune_1 = runMSE(OM_tune_1, Hist=T)                          # save hist object

OM_tune_2 = SubCpars(OM,7:12)                                 # tuning OM is just 18 simulations
Hist_tune_2 = runMSE(OM_tune_2, Hist=T)                          # save hist object

OM_tune_3 = SubCpars(OM,13:18)                                 # tuning OM is just 18 simulations
Hist_tune_3 = runMSE(OM_tune_3, Hist=T)                          # save hist object

saveRDS(OM_tune_1,"OMs/Performance/BSH_tune_1.rds")
saveRDS(Hist_tune_1,"OMs/Performance/Hist_tune_1.rds")

saveRDS(OM_tune_2,"OMs/Performance/BSH_tune_2.rds")
saveRDS(Hist_tune_2,"OMs/Performance/Hist_tune_2.rds")

saveRDS(OM_tune_3,"OMs/Performance/BSH_tune_3.rds")
saveRDS(Hist_tune_3,"OMs/Performance/Hist_tune_3.rds")



# =========== END OF SCRIPT ====================================================================================================

