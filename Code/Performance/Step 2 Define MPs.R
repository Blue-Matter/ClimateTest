# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# Define MPs and tune

library(openMSE)
setwd('C:/GitHub/ClimateTest') # setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')

source("Code/Performance/Source/MP_tuning.r")
source("Code/Performance/Source/MP_internal.r")


# -=- MP archetypes and derivatives ------------------------------------------------------------------------------
Hist = readRDS('OMs/Performance/Hist.rds')
Hist_tune = readRDS('OMs/Performance/Hist_tune.rds')

Data = Hist@Data; x = 1                                   # Data for designing MPs

# Index target MP
It_5 = function(x, Data, reps = 1, targ = 2, nyrs = 3, maxchng = 0.05, maxTAC = 4E5){
  I = Data@Ind[x,]/mean(Data@Ind[x,66:70],na.rm=T)
  recI = mean(I[length(I)-((nyrs-1):0)])
  mod = recI/targ
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}

# Index ratio MP
Ir_5 = function(x, Data, reps = 1, targ = 0.5, nyrs = 3, maxchng = 0.05, maxTAC = 4E5){
  CpI = mean(Data@Cat[x,66:70]) / mean(Data@Ind[x,39:43],na.rm=T)
  I = Data@Ind[x,]
  recI = mean(I[length(I)-((nyrs-1):0)])
  PropTAC = recI * CpI * targ
  mod = PropTAC / Data@MPrec[x]
  #if(ncol(Data@Cat)==50)saveRDS(Data,"C:/temp/Data.rds")
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}  

# Index slope MP
Is_5 = function(x, Data, reps=1, targ = 0.025, nyrs = 5, fac = 1, maxchng = 0.05, maxTAC = 4E5){
  I = Data@Ind[x,]/mean(Data@Ind[x,66:70],na.rm=T)
  slp = lm(y~x,data=data.frame(x=1:nyrs,y=I[length(I)-((nyrs-1):0)]))$coefficients[[2]]
  mod = exp((slp-targ)*fac)
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}

class(It_5) = class(Ir_5) = class(Is_5) = "MP"

It_10 = It_5
Ir_10 = Ir_5
Is_10 = Is_5

formals(It_10)$maxchng = formals(Ir_10)$maxchng = formals(Is_10)$maxchng = 0.1 # set max TAC change to 10%

class(It_10) = class(Ir_10) = class(Is_10) = "MP"


# --- Tuning --------------------------------------------------------------------------------------

#Hist_list = list(Hist_tune)
Hist_list=list(Hist)
paral = F
#setup(cpus=3)
#sfExport('doRec') # export any functions used by MPs


# A function that calculates the squared difference between obtained and target mean catch  
minfunc = function(MSE_list){ 
  Bm = sapply(MSE_list,function(X){X@SSB[,1,19:21]/X@SSB_hist[,X@nyears]})
  Bm = mean(Bm) 
  cat(paste0("Bm = ",round(Bm,2),"\n"))
  (Bm - 1)^2 # stable biomass on average
}

# A function that calculates the squared difference between obtained and target mean catch  
#minfunc = function(MSE_list){ 
 # Catm = sapply(MSE_list,function(X){mean(X@Catch)})
 # Catw =  mean(Catm) 
  #cat(paste0("Cat = ",round(Catw,2),"\n"))
  #(Catw - 30000)^2 # (30kt)
#}

# A function that calculates the squared difference between obtained and target PGK  
#minfunc = function(MSE_list){ 
#  PGKm = sapply(MSE_list,function(X){mean(X@SB_SBMSY>1 & X@F_FMSY < 1)})
#  PGKw =  mean(PGKm) # ! this should really be mean() but this way it matches default slick table
#  cat(paste0("PGKw = ",round(PGKw,6),"\n"))
#  (PGKw - 0.6)^2 # PGK = 0.6
#}

# Index target MP tuning

bounds = c(1,3)
It_5t = tune_MP(Hist_list,"It_5","targ",bounds,minfunc, tol=1E-3, parallel=paral)
It_10t = tune_MP(Hist_list,"It_10","targ",bounds,minfunc, tol=1E-3, parallel=paral)

saveRDS(It_5t,"MPs/Performance/It_5t.rda")
saveRDS(It_10t,"MPs/Performance/It_10t.rda")

# Index ratio MP tuning

bounds = c(0.3,2.5)
Ir_5t = tune_MP(Hist_list,"Ir_5","targ",bounds,minfunc, tol=1E-3, parallel=paral)
Ir_10t = tune_MP(Hist_list,"Ir_10","targ",bounds,minfunc, tol=1E-3, parallel=paral)

saveRDS(Ir_5t,"MPs/Performance/Ir_5t.rda")
saveRDS(Ir_10t,"MPs/Performance/Ir_10t.rda")

# Index slope MP tuning

bounds = c(-0.05,0.15)
Is_5t = tune_MP(Hist_list,"Is_5","targ",bounds,minfunc, tol=1E-3, parallel=paral)
Is_10t = tune_MP(Hist_list,"Is_10","targ",bounds,minfunc, tol=1E-3, parallel=paral)

saveRDS(Is_5t,"MPs/Performance/Is_5t.rda")
saveRDS(Is_10t,"MPs/Performance/Is_10t.rda")


# --- Check MPs ----------------------------------------------------------------------------------

Hist = readRDS('OMs/Performance/Hist.rds')
allMPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t")
testMSE = Project(Hist, allMPs)
Brel(testMSE)
Pplot(testMSE)
matplot(apply(testMSE@Catch,3:2,mean),type="l")
saveRDS(testMSE,"MSEs/Performance/testMSE.rds")


# =========== END OF SCRIPT ====================================================================================================
