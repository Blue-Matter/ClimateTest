# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 4 Calculate robustness metrics


library(openMSE)
setwd('C:/GitHub/ClimateTest')
setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/PMs.r')

types = c("M", "R", "K") 
ntypes = length(types)
Blist = lapply(1:ntypes,function(X,types)readRDS(paste0("Results/Performance/",types[X],"_PGKstat.rds")),types=types)

# --- set up cluster and calculate MSE results for various climate scenarios -----


Bmetric = sapply(get(MSEobjname),function(X)PGK_stat(X)@Mean)
saveRDS(Bmetric,paste0("Results/Performance/",type,"_PGKstat.rds")) 

Ymetric = sapply(get(MSEobjname),function(X)PGK_stat(X)@Mean)
saveRDS(Ymetric,paste0("Results/Performance/",type,"_Yrel.rds"))



for(tt in 1:ntypes){
  type = names(maxpercs)[tt]
  MSEobjname = paste0("MSEs_",type)
  percs = seq(0,maxpercs[tt],length.out = nval)
  assign(MSEobjname, CT_perf(list(OM), MPs, type, percs, horizon))
  saveRDS(get(MSEobjname),paste0("MSEs/Performance/",MSEobjname,".rds"))
  cat(paste0("Completed ", type," (",tt,"/",ntypes,")"))
}

sfStop()




# natural mortality rate
# !!! send vector of percentages to the CT_perf function instead !!!
# !!! edit PGK to be non dynamic !!!


MSEs_M = 

sapply(MSEs_M,function(X)PGK_dyn(X)@Mean)
sapply(MSEs_M,function(X)PGK_stat(X)@Mean)





# =========== END OF SCRIPT ====================================================================================================
