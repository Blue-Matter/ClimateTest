# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# Calculate robustness metrics


library(openMSE)
setwd('C:/GitHub/ClimateTest')
setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/OM_mod.r')
source('Code/Performance/source/PMs.r')
source("Code/Performance/Source/MP_internal.r")


# --- Load MPs -------------------------------------------------------------------

MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t")
for(MP in seq_along(MPs))assign(MPs[MP],readRDS(paste0("MPs/Performance/",MPs[MP],".rda")))


# --- Load OMs -------------------------------------------------------------------

OM = readRDS('OMs/Performance/BSH.rds')


# --- set up cluster and calculate MSE results for various climate scenarios -----

nval = 9
horizon = 20
maxpercs = c(M = 27)

ntypes = length(maxpercs)

for(tt in 1:ntypes){
  type = names(maxpercs)[tt]
  MSEobjname = paste0("MSEs_",type)
  percs = seq(0,maxpercs[tt],length.out = nval)
  assign(MSEobjname, CT_perf(list(OM), MPs, type, percs, horizo
  saveRDS(get(MSEobjname),paste0("MSEs/Performance/",MSEobjname,".rds"))
  cat(paste0("Completed ", type," (",tt,"/",ntypes,")"))
}


setup(cpus = nval)

sfExport('doRec') # export any functions used by MPs
sfExport(list = MPs)

# natural mortality rate
# !!! send vector of percentages to the CT_perf function instead !!!
# !!! edit PGK to be non dynamic !!!


MSEs_M = 

sapply(MSEs_M,function(X)PGK_dyn(X)@Mean)
sapply(MSEs_M,function(X)PGK_stat(X)@Mean)





# =========== END OF SCRIPT ====================================================================================================
