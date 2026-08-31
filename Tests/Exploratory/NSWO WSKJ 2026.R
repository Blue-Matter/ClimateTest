# ==============================================================================================================================
# ====== SCRS/2026/XXX Exploratory test for WSKJ and NSWO ======================================================================
# ==============================================================================================================================

# Tom Carruthers
# Jan 2026

# ===== Install  =========================================================================================

# remotes::install_github('blue-matter/ClimateTest') # tested with 0.3.40


# ===== All case studies =================================================================================

library('ClimateTest')        # ClimateTest functions and demo objects
packageVersion('ClimateTest') # Check ClimateTest version
setup()                       # Set up parallel processing
outdir ="C:/GitHub/ClimateTest/Tests/Exploratory"
setwd("C:/GitHub/ICCAT_WSKJ_MSE_2025")
horizon = 25
RT = 0.8

# ===== NSWO case study =============================================================================

# Prequisites ---------------------

library(SWOMSE)                                       # Load N. Atl. swordfish MSE library


# Step 1 --------------------------

# small_OM = SubCpars(MOM_005,1:8) # small version for testing
# OM_list = list(MOM_005)         # small version for testing
OM_list = list(MOM_005)
Hist_list = CT_1_prep(OM_list) # Spool up with no climate effects


# Step 2 ---------------------------


MPs_tuned = CT_2_tune(Hist_list,
                      MPs = c("CE_b","MCC11_b"),      # Names of MPs to be tuned and tested
                      MP_par_nams = rep("tunepar",2), # tuning parameter names for each MP
                      type = "SSB",                   # Spawning stock biomass tuning,
                      horizon,                        # Returns a list of tuned MPs
                      parallel=F)                     # No need to run in parallel (only 1 OM)

saveRDS(MPs_tuned, paste0(outdir,"/MPs_NSWO.rds"))

MPs_tuned = readRDS(paste0(outdir,"/MPs_NSWO.rds"))

# Step 3 ---------------------------

setup(cpus=8)                      # Initialize cluster for parallel computation
sfLibrary(SWOMSE)            # Send SWOMSE objects and functions to the cluster
tests = c(K = 75,            # Up to 75% decreases in somatic growth
          C = 75,            # Up to 75% decrease in condition factor (W / L^b)
          M = 25,            # Up to 25% increases in natural mortality
          R = 50             # Up to 50% decreases in recruitment strength
)

nlev = 8                     # Number of increments for each test (to interpolate over)

CT_data = CT_3_test(Hist_list, MPs_tuned, nlev, horizon, tests)

saveRDS(CT_data, "C:/temp/CT_data_NSWO.rds")
CT_data = readRDS("C:/temp/CT_data_NSWO.rds")

# Step 4 ---------------------------

results = CT_metrics(CT_data, horizon=horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                           # SSB loss metrics
tab = CT_tabulate(results$SSB_relative, RT=RT) # Calculate the test level where RT is crossed
makeCTtab(tab)                                 # HTML table
CT_proj(CT_data, horizon, "M", RT=RT)          # Plot the projected SSB outcomes and test level
CT_4_summary(CT_data, RT = RT, horizon = horizon) # Summary plot of all tests


# ===== WSKJ case study =============================================================================

# WSKJ Objects
Hist = readRDS("03_Hists/OM005_IVInds_ver03.hist") # Operating Model
sapply(paste0('04_MPs/',c('IR.R','SP.R','SPAH.R')),source)
source("06_script_MP_Internal_Functions_ver00.R")

# Step 1 ---------------------------
#Hist_list = CT_1_prep(OM_list) # Spool up with no climate effects (can't do it here because we need some of the Hist stuff)
Hist_list = list(Hist)

# Step 2 ---------------------------
MP_par_intervals = list(c(0.005,0.02), c(0.55,0.56), c(0.72,0.78))
MPs_tuned = CT_2_tune(Hist_list,
                      MPs = c("SPAH","SP","IR"),      # Names of MPs to be tuned and tested
                      MP_par_nams = rep("tunepar",3), # tuning parameter names for each MP
                      MP_par_intervals = MP_par_intervals,
                      type = "SSB",                   # Spawning stock biomass tuning,
                      horizon,
                      near_enough = 1E-3,
                      tol = 0.01,
                      parallel=F)                     # No need to run in parallel (only 1 OM)

saveRDS(MPs_tuned, paste0(outdir,"/MPs_WSKJ.rds"))

MPs_tuned = readRDS(paste0(outdir,"/MPs_WSKJ.rds"))

# Step 3 ---------------------------

setup()                      # Initialize cluster for parallel computation
sfExport(list=c("FixedTAC","SameTAC","adjust_TAC","adjust_TAC2","WSKJ_Data","Assumed2025Catch","Catchdf")) # export internal MP function from: 06_script_MP_Internal_Functions_ver00.R
tests = c(K = 75,            # Up to 75% decreases in somatic growth
          C = 50,            # Up to 50% decrease in condition factor (W / L^b)
          M = 10,            # Up to 10% increases in natural mortality
          R = 25)            # Up to 25% decreases in recruitment strength

nlev = 8                     # Number of increments for each test (to interpolate over)

CT_data = CT_3_test(Hist_list, MPs_tuned, nlev, horizon, tests)

saveRDS(CT_data, "C:/temp/CT_data_WSKJ.rds")


# Step 4 ---------------------------

RT = 0.8

results = CT_metrics(CT_data, horizon=horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                           # SSB loss metrics

tab = CT_tabulate(results$SSB_relative, RT=RT) # Calculate the test level where RT is crossed
makeCTtab(tab)                                 # HTML table

CT_proj(CT_data, horizon, "M", RT=RT)          # Plot the projected SSB outcomes and test level

CT_4_summary(CT_data, RT = RT, horizon = horizon) # Summary plot of all tests


# === Make some figures ===========================================================

library(kableExtra)
library(webshot2)
CT_NSWO = readRDS("C:/temp/CT_data_NSWO.rds")
CT_WSKJ = readRDS("C:/temp/CT_data_WSKJ.rds")

setwd("C:/GitHub/ClimateTest/Tests/Exploratory")

# --- NSWO -------
jpeg("Figures/Summary_NSWO.jpg",res=400,height=9, width=7,units='in')
  CT_4_summary(CT_NSWO, RT = RT, horizon = horizon) # Summary plot of all tests
dev.off()

results = CT_metrics(CT_NSWO, horizon=horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                           # SSB loss metrics
tab = CT_tabulate(results$SSB_relative, RT=RT) # Calculate the test level where RT is crossed
tabf = makeCTtab(tab)
# save_kable(tabf,"Figures/sumtab.jpg")

jpeg("Figures/M_proj_NSWO.jpg",res=400,height=7.7, width=8,units='in')
  CT_proj(CT_NSWO, horizon, "M", RT=RT)          # Plot the projected SSB outcomes and test level
dev.off()

# --- WSKJ --------
jpeg("Figures/Summary_WSKJ.jpg",res=400,height=9, width=7,units='in')
  CT_4_summary(CT_WSKJ, tests=c("K","C","M","R"), RT = RT, horizon = horizon) # Summary plot of all tests
dev.off()


results = CT_metrics(CT_WSKJ, horizon=horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                           # SSB loss metrics
tab = CT_tabulate(results$SSB_relative, subset=c(1,2,4,5), RT=RT) # Calculate the test level where RT is crossed
tabf = makeCTtab(tab)
# save_kable(tabf,"Figures/sumtab.jpg")

jpeg("Figures/M_proj_WSKJ.jpg",res=400,height=9.5, width=8,units='in')
 CT_proj(CT_WSKJ, horizon, "M", RT=RT)          # Plot the projected SSB outcomes and test level
dev.off()


# ==============================================================================================================================
# ========= END ================================================================================================================
# ==============================================================================================================================








