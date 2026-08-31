# ==============================================================================================================================
# ====== EXAMPLE CODE FROM THE SPLASH PAGE =====================================================================================
# ==============================================================================================================================

# Tom Carruthers
# Dec 2025

# ===== Install  =========================================================================================

remotes::install_github('blue-matter/ClimateTest')


# ===== Package case studies ============================================================================

# Prerequisites ------------

library('ClimateTest')        # ClimateTest functions and demo objects
packageVersion('ClimateTest') # Check ClimateTest version
setup()                       # Set up parallel processing


# Step 1 -------------------

OM_list = list(BET_1, BET_2)       # A list of operating models
Hist_list = CT_1_prep(OM_list)     # Same OMs but without climate impacts and included historical reconstruction


# Step 2 -------------------

Ir1 = Ir2 = ClimateTest::Ir  # Generic Index ratio MP from Climate Test package
formals(Ir2)$maxchng = 0.15  # TAC updates for Ir2 can now vary by 15% (default is 10%)
It = ClimateTest::It         # Generic Index target MP from Climate Test package

# Tuning options
horizon = 30                 # Tuned to be same as current in 30 projected years

# Tuning: defaults to MP parameter 'tune' over range 1/3x to 3x the default MP argument value
MPs_tuned = CT_2_tune(Hist_list,
                      MPs = c("Ir1","Ir2","It"), # Names of MPs to be tuned and tested
                      type = "SSB",              # Spawning stock biomass tuning
                      horizon)                   # SSB is same as current in 30 years


# Step 3 -------------------

tests = c(S = 200,           # Up to 200% increase in catchability over the specified horizon
          M = 25,            # Up to 25% increases in natural mortality over the specified horizon
          R = 50,            # Up to 50% decreases in recruitment strength over the specified horizon
          K = 75,            # Up to 75% decreases in somatic growth over the specified horizon
          C = 75)            # Up to 75% decrease in condition factor (W / L^b) over the specified horizon

nlev = 8                     # Number of levels for each test (to interpolate over)

CT_data = CT_3_test(Hist_list,  # From Step 1
                    MPs_tuned,  # From Step 2
                    nlev, horizon, tests) # Over 8 increments for 30 projected years


# Step 4 -------------------

results = CT_metrics(CT_data, horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                # SSB loss metrics across tests and MPs

# Robustness metrics
RT = 0.85                # Robustness threshold, when SSB drops below this fraction of starting SSB
tab = CT_tabulate(results$SSB_relative, RT)          # Calculate the test level where RT is crossed
dt = makeCTtab(tab)                                  # HTML table
dt
CT_proj(CT_data, horizon, "M", RT = RT)              # Explanatory figure
CT_4_summary(CT_data, RT = RT, horizon = horizon)    # Summary figure



# ===== SWO case study =============================================================================

# Prequisites ---------------------

library('ClimateTest')        # ClimateTest functions and demo objects
packageVersion('ClimateTest') # Check ClimateTest version
setup()                       # Set up parallel processing
library(SWOMSE)                                       # Load N. Atl. swordfish MSE library


# Step 1 --------------------------

small_OM = SubCpars(MOM_005,1:8)
OM_list = list(small_OM)
Hist_list = CT_1_prep(OM_list) # Spool up with no climate effects


# Step 2 ---------------------------

horizon = 25
MPs_tuned = CT_2_tune(Hist_list,
                      MPs = c("CE_b","MCC11_b"),      # Names of MPs to be tuned and tested
                      MP_par_nams = rep("tunepar",2), # tuning parameter names for each MP
                      type = "SSB",                   # Spawning stock biomass tuning,
                      horizon,                        # Returns a list of tuned MPs
                      parallel=F)                     # No need to run in parallel (only 1 OM)


# Step 3 ---------------------------

setup()                      # Initialize cluster for parallel computation
sfLibrary(SWOMSE)            # Send SWOMSE objects and functions to the cluster
tests = c(C = 75,            # Up to 75% decrease in condition factor (W / L^b)
          S = 200,           # Up to 200% increase in catchability
          M = 25,            # Up to 25% increases in natural mortality
          R = 50,            # Up to 50% decreases in recruitment strength
          K = 75)            # Up to 75% decreases in somatic growth

nlev = 8                     # Number of increments for each test (to interpolate over)

CT_data = CT_3_test(Hist_list, MPs_tuned, nlev, horizon, tests)


# Step 4 ---------------------------


RT = 0.8

results = CT_metrics(CT_data, horizon=horizon) # Calculate SSB and yield loss metrics
results$SSB_relative                           # SSB loss metrics

tab = CT_tabulate(results$SSB_relative, RT=RT) # Calculate the test level where RT is crossed
makeCTtab(tab)                                 # HTML table

CT_proj(CT_data, horizon, "M", RT=RT)          # Plot the projected SSB outcomes and test level

CT_4_summary(CT_data, RT = RT, horizon = horizon) # Summary plot of all tests



# ==============================================================================================================================
# ========= END ================================================================================================================
# ==============================================================================================================================








