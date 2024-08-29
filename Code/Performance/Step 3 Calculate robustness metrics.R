# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# Calculate robustness metrics


library(openMSE)
setwd('C:/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/OM_mod.r')
source('Code/Performance/source/PMs.r')


# --- Load MPs -------------------------------------------------------------------

MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t")
for(MP in seq_along(MPs))assign(MPs[MP],readRDS(paste0("MPs/Performance/",MPs[MP],".rda")))


# --- Load OMs -------------------------------------------------------------------

OM = readRDS('OMs/Performance/BSH.rds')


# --- set up cluster and calculate MSE results for various climate scenarios -----


setup(cpus=7)
sfExport('doRec') # export any functions used by MPs
sfExport(list = MPs)

# natural mortality rate
# !!! send vector of percentages to the CT_perf function instead !!!
# !!! edit PGK to be non dynamic !!!

MSEs_M = CT_perf(list(OM), MPs, type = "M", maxperc = 18, horizon=20, nval = 7, parallel = T)
class(MSEs_M[[1]])
sapply(MSEs_M,function(X)PGK(X)@Mean)





# =========== END OF SCRIPT ====================================================================================================
