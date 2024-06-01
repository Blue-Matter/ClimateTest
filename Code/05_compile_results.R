
# === Compile results ==========================================================

# Tom Carruthers 
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)

Snames = Snames = c("BET","SWO","BSH")
ns = length(Snames)

test_name = c("Ref",paste0(rep(c("M","CF","Rec","K"),each=2),"_",rep(c("Mod","Ext"),4)))
test_type = c("No_ch","M_ch","M_ch","CF_ch","CF_ch","Rec_ch","Rec_ch","K_ch","K_ch")
bys =     c(0,       10,    20,    -10,    -20,    -20,     -40,     -10,   -20)
nt = length(test_type)

MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat","SP_MSY","SP_4010")
runs = expand.grid(1:ns,1:nt)
runfiles = paste0("MSEs_wAssess/",Snames[runs[,1]],"_",test_name[runs[,2]],".rds")
nr = nrow(runs)

getPerf = function(x, Refyr = 30){
  MSE = readRDS(x)
  if(class(MSE)=="MSE"){
    Brel = MSE@SB_SBMSY[,,Refyr]
    Chist = MSE@CB_hist
    Crel = MSE@Catch[,,Refyr] / Chist[,ncol(Chist)]
    return(list(Crel = Crel, Brel = Brel))
  }else{
    return(list(Crel = NA, Brel = NA))
  }
}

sfLibrary(openMSE)
Res = sfLapply(runfiles,getPerf)

Crel = as.vector(t(sapply(Res,function(x)x$Crel)))
Brel = as.vector(t(sapply(Res, function(x)x$Brel)))


nsim = MSE@nsim
nMPs = MSE@nMPs
index = expand.grid(1:nsim,1:nMPs,1:ns,1:nt)

Sim = index[,1]
MP = MSE@MPs[index[,2]]
Stock = Snames[index[,3]]
Test = test_name[index[,4]]

Cdat = data.frame(Sim,MP,Test,Stock,Crel)
Bdat = data.frame(Sim,MP,Test,Stock,Brel)

for(i in 1:4){
  Cdat[,i] = as.factor(Cdat[,i])
  Bdat[,i] = as.factor(Bdat[,i])
}

saveRDS(Cdat,"Results/Cdat.rds")
saveRDS(Bdat,"Results/Bdat.rds")


# === End of script ============================================================



