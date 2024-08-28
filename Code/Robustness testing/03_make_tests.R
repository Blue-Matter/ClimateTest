# === Make demo climate tests ==================================================

# Tom Carruthers 
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)
design = F

# --- Load an OM object for designing tests-------------------------------------

if(design){
  OM = readRDS("OMs/OM_BET.rds")
  cpars = OM@cpars
}

# --- 

No_ch = function(OM,by=10,by_yr=30)return(OM)

# generic % change modifier

ChangePro_3D = function(OM, slot="M_ageArray",by = 10,by_yr = 30){
  mat = OM@cpars[[slot]]
  dims = dim(mat) #; print(dims)
  nsim = dims[1]
  na = dims[2]
  ny = dims[3]
  yind = ny - ((OM@proyears-1):0)
  perc = getperc(by,by_yr)
  mult = (1+perc)^(1:OM@proyears)
  multarr = array(rep(mult,each=nsim*na),c(nsim,na,OM@proyears))
  OM@cpars[[slot]][,,yind] = mat[,,yind]*multarr
  OM
}

getperc = function(by,by_yr){
  frac = 1 + by/100
  frac^(1/by_yr)-1
}

ChangePro_2D = function(OM, slot="Perr_y",by = 10,by_yr = 30){
  mat = OM@cpars[[slot]]
  dims = dim(mat); print(dims)
  nsim = dims[1]
  ny = dims[2]
  yind = ny - ((OM@proyears-1):0)
  perc = getperc(by,by_yr)
  mult = (1+perc)^(1:OM@proyears)
  multarr = array(rep(mult,each=nsim),c(nsim,OM@proyears))
  OM@cpars[[slot]][,yind] = mat[,yind]*multarr
  OM
}

M_ch = function(OM, by = 10, by_yr = 30)   ChangePro_3D(OM, slot = "M_ageArray",by=by,by_yr = by_yr)
CF_ch = function(OM, by = -10, by_yr = 30)  ChangePro_3D(OM, slot = "Wt_age",by=by, by_yr = by_yr)
Rec_ch = function(OM, by = -25, by_yr = 30) ChangePro_2D(OM, slot = "Perr_y",by=by, by_yr = by_yr)

K_ch = function(OM, by = -20, by_yr = 30, Kstep = 1){
  na = OM@maxage
  proyears = OM@proyears
  nyears = OM@nyears
  nsim = OM@nsim
  vbK = array(OM@cpars$K*Kstep,c(nsim,na+1,proyears))
  Linf = array(OM@cpars$Linf,c(nsim,na+1,proyears))
  t0 = OM@t0[1]
  perc = getperc(by,by_yr)
  mult = (1+perc)^(1:OM@proyears)
  multarr = array(rep(mult,each=nsim*(na+1)),c(nsim,na+1,proyears))
  newK = vbK  *multarr
  agearr = aperm(array((0:na),c(na+1,nsim,proyears)),c(2,1,3))
  La = Linf * (1-exp(-newK *(agearr-t0)))
  OM@cpars$Len_age[,,nyears+1:proyears] = La
  OM@cpars$Len_age[OM@cpars$Len_age<=0] = 1E-3
  OM@cpars$Wt_age = OM@a[1]*OM@cpars$Len_age^OM@b[1]
  OM
}

class(No_ch) = class(M_ch) = class(CF_ch) = class(Rec_ch) = class(K_ch) = "ClimateTest"

if(design){
  # tests 
  OM2 = M_change(OM)
  OM@cpars$M_ageArray[1,1,]
  OM2@cpars$M_ageArray[1,1,]
  
  OM2 = CF_change(OM)
  OM@cpars$Wt_age[1,1,]
  OM2@cpars$Wt_age[1,1,]
  
  OM2 = Rec_change(OM)
  OM@cpars$Perr_y[1,]
  OM2@cpars$Perr_y[1,]
  
  OM2 = K_change(OM)
  OM@cpars$Len_age[1,11,OM@nyears+1:OM@proyears]
  OM2@cpars$Len_age[1,11,OM@nyears+1:OM@proyears]
}


cat("Demo functions of class ClimateTest loaded: No_ch, M_ch, CF_ch, Rec_ch, K_ch \n")

# === End of script ===========================================================
