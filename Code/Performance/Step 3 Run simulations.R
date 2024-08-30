# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 3 Run simulations 


library(openMSE)
setwd('C:/GitHub/ClimateTest') #  setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/OM_mod.r')
source('Code/Performance/source/PMs.r')
source("Code/Performance/Source/MP_internal.r")


# --- Load MPs -------------------------------------------------------------------

MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t")
for(MP in seq_along(MPs))assign(MPs[MP],readRDS(paste0("MPs/Performance/",MPs[MP],".rda")))


# --- Load OMs -------------------------------------------------------------------

OM = readRDS('OMs/Performance/BET.rds')


# --- set up cluster and calculate MSE results for various climate scenarios -----
nval = 7
setup(cpus = nval)
sfExport('doRec') # export any functions used by MPs
sfExport(list = MPs)


horizon = 20
maxpercs = c(M = 12, R = 36, K = 24, S = 180, C = 60)
ntypes = length(maxpercs)

for(tt in 1:ntypes){

  type = names(maxpercs)[tt]
  MSEobjname = paste0("MSEs_",type)
  percs = seq(0,maxpercs[tt],length.out = nval)
  assign(MSEobjname, CT_perf(OM_list=list(OM), MPs, type, percs, horizon))
  saveRDS(get(MSEobjname),paste0("C:/temp/ClimateTest/",MSEobjname,".rds")) # too large
  
  Bmetric = sapply(get(MSEobjname),function(X)Brel(X)@Mean)
  Ymetric = sapply(get(MSEobjname),function(X)Yrel(X)@Mean)
  rownames(Bmetric) = rownames(Ymetric) = MPs
  colnames(Bmetric) = colnames(Ymetric) = percs
  saveRDS(Bmetric,paste0("Results/Performance/",type,"_PGKstat.rds")) 
  saveRDS(Ymetric,paste0("Results/Performance/",type,"_Yrel.rds")) 
  
  cat(paste0("Completed ", type," (",tt,"/",ntypes,")"))
  
}

sfStop()




# =========== END OF SCRIPT ====================================================================================================
