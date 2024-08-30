# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 4 Calculate robustness metrics


library(openMSE)
setwd('C:/GitHub/ClimateTest')
setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/PMs.r')

types = c("M", "R", "K","S","C") 
ntypes = length(types)
Blist = lapply(1:ntypes,function(X,types)readRDS(paste0("Results/Performance/",types[X],"_PGKstat.rds")),types=types)
Blist



# =========== END OF SCRIPT ====================================================================================================
