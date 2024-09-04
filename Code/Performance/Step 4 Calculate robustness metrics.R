# ===============================================================================================================================
# ========== Climate Test Performance Metrics ===================================================================================
# ===============================================================================================================================

# 4 Calculate robustness metrics

# !!!!!!!!!!!!!!1 This is also in docs/Index.rmd !!!!!!!!!!!!!!1111

library(openMSE)
library(kableExtra)
library(viridis)
library(DT)

setwd('C:/GitHub/ClimateTest')
setwd('C:/Users/tcarruth/Documents/GitHub/ClimateTest')


# --- Source code for OM modifications and performance metrics -------------------

source('Code/Performance/source/Results.r')


#  --- Results Table -------------------------------------------------------------

types = c("M", "R", "K","S","C") 
desc = c("Natural mortality","Recruitment level", "Somatic growth", "Spatial / catchability","Condition factor")
ntypes = length(types)
Blist = lapply(1:ntypes,function(X,types)readRDS(paste0("Results/Performance/",types[X],"_PGKstat.rds")),types=types)
names(Blist) = types
MPdesc = paste(rep(c("Index target","Index ratio","Index slope"),each=2),c("5% change","10% change"))
tab = CT_tabulate(Blist, targlev=0.7, subset = c(1,2,3,5))
tab = cbind(Management_procedure = MPdesc,tab )
dt=makeCTtab(tab)
dt

# --- Explanatory Figure ---------------------------------------------------------

type = "K"
MSEs = readRDS(paste0("C:/temp/ClimateTest/MSEs_",type,".rds"))
nM = length(MSEs)
Blisty = Blist[[match(type,types)]]
levs = colnames(Blisty)
MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3))

  
MPnos = 1:2
nMP = length(MPnos)

jpeg("Figures/Performance/Explanatory.jpg",res=400,width=8,height=7,units='in')

  par(mfrow=c(nMP,2),mai = c(0.4,0.4,0.4,0.01),omi=c(0.25,0.25,0.01,0.01))
  mpadj = c(-2,-22)
  for(mm in MPnos){
    cols = viridis(nM,begin=1,end=0)
    Bio = sapply(MSEs,function(X,mm,rng)c(mean(X@SSB_hist[,X@proyears]),apply(X@SSB[,mm,rng],2,mean)),mm=mm,rng=1:21)
    Bio = Bio / 1E3
    CT_proj_plot(Bio,cols,levs, ref=0.7,nextra=6, CurYr = 2019, Horizon = 22,nyplot = 20)
    mtext(paste0(MPdesc[mm]," MP (",MPs[mm],")"),3,line=mpadj[mm],outer=T)
    CT_intplot(Blisty,mm,MPs,type,levs,Bio,cols,refyr = 22)
    mtext("Spawning stock biomass (kt)",2,line=0.175,outer=T)
    mtext(c("Projection Year","% Decline in K After 20 Years"),1,adj=c(0.25,0.92),line=0.2,outer=T)
  }
  
dev.off()

# =========== END OF SCRIPT ====================================================================================================
