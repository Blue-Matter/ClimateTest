# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# Define MPs and tune

library(openMSE)
setwd('C:/GitHub/ClimateTest')
source("Code/Performance/Source/MP_tuning.r")


# -=- MP archetypes and derivatives ------------------------------------------------------------------------------

Hist_tune_1 = readRDS('OMs/Performance/Hist_tune_1.rds')
Hist_tune_2 = readRDS('OMs/Performance/Hist_tune_2.rds')
Hist_tune_3 = readRDS('OMs/Performance/Hist_tune_3.rds')

Data = Hist_tune_1@Data; x = 1                                   # Data for designing MPs

# calculates a TAC from a TAC modifier, maximum TAC changes and maxTAC
doRec = function(MPrec, mod, maxchng, maxTAC){ 
  if(mod > (1+maxchng))mod = 1+maxchng
  if(mod < (1-maxchng))mod = 1-maxchng
  Rec = new('Rec')
  Rec@TAC = min(MPrec*mod, maxTAC)
  Rec
}

# Index target MP
It_5 = function(x, Data, reps = 1, targ = 2, nyrs = 3, maxchng = 0.05, maxTAC = 4E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  recI = mean(I[length(I)-((nyrs-1):0)])
  mod = recI/targ
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}

# Index ratio MP
Ir_5 = function(x, Data, reps = 1, targ = 0.5, nyrs = 3, maxchng = 0.05, maxTAC = 4E5, Ind =9){
  CpI = mean(Data@Cat[x,39:43]) / mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  I = Data@AddInd[x,Ind,]
  recI = mean(I[length(I)-((nyrs-1):0)])
  PropTAC = recI * CpI * targ
  mod = PropTAC / Data@MPrec[x]
  #if(ncol(Data@Cat)==50)saveRDS(Data,"C:/temp/Data.rds")
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}  

# Index slope MP
Is_5 = function(x, Data, reps=1, targ = 0.025, nyrs = 5, fac = 1, maxchng = 0.05, maxTAC = 4E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
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

Hist_list = list(Hist_tune_1, Hist_tune_2, Hist_tune_3)
setup(cpus=3)
sfExport('doRec') # export any functions used by MPs

# A function that calculates the squared difference between obtained and target mean catch  
minfunc = function(MSE_list){ 
  Catm = sapply(MSE_list,function(X){mean(X@Catch)})
  Catw =  mean(Catm) 
  cat(paste0("Cat = ",round(Catw,2),"\n"))
  (Catw - 22000)^2 # (22kt)
}
minfunc = function(MSE_list){ 
  PGKm = sapply(MSE_list,function(X){mean(X@SB_SBMSY>1 & X@F_FMSY < 1)})
  PGKw =  mean(PGKm) # ! this should really be mean() but this way it matches default slick table
  cat(paste0("PGKw = ",round(PGKw,6),"\n"))
  (PGKw - 0.6)^2
}

# Index target MP tuning

bounds = c(1,2)
It_5t = tune_MP(Hist_list,"It_5","targ",bounds,minfunc, tol=1E-3, parallel=T)
It_10t = tune_MP(Hist_list,"It_10","targ",bounds,minfunc, tol=1E-3, parallel=T)

saveRDS(It_5t,"MPs/Performance/It_5t.rda")
saveRDS(It_10t,"MPs/Performance/It_10t.rda")

# Index ratio MP tuning

bounds = c(0.3,1.2)
Ir_5t = tune_MP(Hist_list,"Ir_5","targ",bounds,minfunc, tol=1E-3, parallel=T)
Ir_10t = tune_MP(Hist_list,"Ir_10","targ",bounds,minfunc, tol=1E-3, parallel=T)

saveRDS(Ir_5t,"MPs/Performance/Ir_5t.rda")
saveRDS(Ir_10t,"MPs/Performance/Ir_10t.rda")

# Index slope MP tuning

bounds = c(0,0.05)
Is_5t = tune_MP(Hist_list,"Is_5","targ",bounds,minfunc, tol=1E-3, parallel=T)
Is_10t = tune_MP(Hist_list,"Is_10","targ",bounds,minfunc, tol=1E-3, parallel=T)

saveRDS(Is_5t,"MPs/Performance/Is_5t.rda")
saveRDS(Is_10t,"MPs/Performance/Is_10t.rda")


# --- Check MPs ----------------------------------------------------------------------------------

Hist = readRDS('OMs/Performance/Hist.rds')
allMPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t")
testMSE = Project(Hist, allMPs)
Pplot(testMSE)
matplot(apply(testMSE@Catch,3:2,mean),type="l")


# =========== END OF SCRIPT ====================================================================================================
