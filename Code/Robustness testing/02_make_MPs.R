# === Make archetypal MPs ======================================================

# Tom Carruthers 
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)
design = F

# --- Make a data object for testing MPs ---------------------------------------

if(design){
  OM = readRDS("OMs/OM_BET.rds")
  MPmakedat = function(x,Data,reps=1){saveRDS(Data,"Sim_data/Example_sim_data.rds"); stop()}; class(MPmakedat) = "MP"
  suppressWarnings(runMSE(OM,"MPmakedat",checkMPs = F))
  Data = readRDS("Sim_data/Example_sim_data.rds")
  x = 1
}

# --- MP1 Size limit at length at 50% fraction mature --------------------------
SzMat = matlenlim2



# --- MP2 Index target setting TAC ---------------------------------------------
IMP_core = function (x, Data, reps = 1, plot = FALSE, delta=15, enp.mult = 0.15,
                     type = "Ftarget",lever="TAE"){
 
  smooth<-function(xx,plot=F,enp.mult=0.15,plotname=""){
    tofill<-!is.na(xx)
    xx[xx==0]<-1E3
    predout<-rep(NA,length(xx))
    dat<-data.frame(x=1:length(xx),y=log(xx))
    enp.target<-sum(tofill)*enp.mult
    out<-loess(y~x,dat=dat,enp.target=enp.target)
    predout[tofill]<-exp(predict(out))
    if(plot){
      plot(xx,type="p",xlab="x",ylab="y",main=plotname)
      lines(predout,col="#ff000090",lwd=2)
    }
    predout
  }
  
  y = max(Data@Year) - Data@LHYear + 1
  nyears = length(Data@Misc$FleetPars$Find[x, ])
  SSBMSY = Data@Misc$ReferencePoints$ReferencePoints$SSBMSY[x]
  SSB = apply(Data@Misc$StockPars$SSB[x,,,],2,sum)
  MSY = Data@Misc$ReferencePoints$ReferencePoints$MSY[x]
  UMSYrat = MSY/(SSBMSY+MSY)
  SSB_P = sum(Data@Misc$StockPars$SSB_P[x,,y,])
  Fref = Data@Misc$FleetPars$Find[,nyears]
 
  Index = Data@Ind[x,]
  ind = nyears - 19:0
  qs = mean(SSB[ind]) / mean(Index[ind])
  
  SSBts = Index * qs
  SSBtss = smooth(SSBts,plot=F,enp.mult = enp.mult)
  SSBest = SSBtss[length(SSBtss)]
  last_TAC = Data@MPrec[x]
  Rec = new('Rec')
  
  if(lever =="TAC"){
   
    if(type == "Itarget"){
      adj = SSBest/SSBMSY
    }else if(type == "Ftarget"){
      trial_TAC = SSBest * UMSYrat
      adj = trial_TAC / last_TAC
    }
    if(adj < 1-(delta/100))adj = 1-(delta/100)
    if(adj > 1+(delta/100))adj = 1+(delta/100)
    new_TAC = last_TAC * adj
    Rec@TAC = new_TAC
  
  }else if(lever =="Spatial"){
    
    adj = SSBMSY/SSBest
    if(adj < 1-(delta/100))adj = 1-(delta/100)
    if(adj > 1+(delta/100))adj = 1+(delta/100)
    Rec@Spatial=c(1,max(1,adj)) # second area is closed in proportion to SSB/SSBMSY
    
  }else if(lever == "TAE"){
    
    if(y == 1) last_eff = 1
    if(y > 1) last_eff = Data@MPrec[x]
   
    if(type == "Itarget"){
      adj = SSBMSY/SSBest
    }else if(type == "Ftarget"){
      catch_ts = smooth(Data@Cat[x,],plot=F)
      catch = catch_ts[length(catch_ts)]
      curU = catch / SSBest
      adj = curU / UMSYrat
    }
    if(adj < 1-(delta/100))adj = 1-(delta/100)
    if(adj > 1+(delta/100))adj = 1+(delta/100)
    
    Rec@Effort = last_eff * adj
  }
  
  Rec
}

ITE = ITC = IRE = IRC = SpC = IMP_core

formals(ITE)$type = "Itarget"
formals(ITE)$lever = "TAE"

formals(ITC)$type = "Itarget"
formals(ITC)$lever = "TAC"

formals(IRE)$type = "Ftarget"
formals(IRE)$lever = "TAE"

formals(IRC)$type = "Ftarget"
formals(IRC)$lever = "TAC"

formals(SpC)$type = "Itarget"
formals(SpC)$lever = "Spatial"

class(ITE) = class(ITC) = class(IRE) = class(IRC) = class(SpC) = "MP"

cat("Demo MPs loaded: ITC, ITE, IRE, IRC, SpC and SzMat \n")

# === End of script ===========================================================
