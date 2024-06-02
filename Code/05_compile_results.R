
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
nt = length(test_type)

MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat","SP_MSY","SP_4010")
runs = expand.grid(1:ns,1:nt)
runfiles = paste0("MSEs/",Snames[runs[,1]],"_",test_name[runs[,2]],".rds")
nr = nrow(runs)

getPerf = function(x, Refyr = 49){
  MSE = readRDS(x)
  if(class(MSE)=="MSE"){
    Brel = MSE@SB_SBMSY[,,Refyr]
    Chist = MSE@CB_hist
    Crel = MSE@Catch[,,Refyr] / Chist[,ncol(Chist)]
    return(list(Crel = Crel, Brel = Brel))
  }else{
    return(list(Crel = NA, Brel = NA,nam = MSE@Name))
  }
}

MSE = readRDS(runfiles[1])
sfLibrary(openMSE)
Res = sfLapply(runfiles,getPerf)
lapply(Res,function(x)x$nam)

Crel = as.vector(sapply(Res,function(x)x$Crel))
Brel = as.vector(sapply(Res, function(x)x$Brel))


nsim = MSE@nsim
nMPs = MSE@nMPs
#index = expand.grid(1:nMPs,1:nsim,1:nt,1:ns)

index = expand.grid(1:nsim,1:nMPs,1:ns,1:nt)

Sim = index[,1]
MP = MSE@MPs[index[,2]]
Stock = Snames[index[,3]]
Test = test_name[index[,4]]

Bdat = data.frame(Sim,MP,Test,Stock,Brel,Crel)
pfunc =function(x) mean(x<0.5,na.rm=T)
Bagg = aggregate(Bdat$Brel,list(MP=Bdat$MP,Test=Bdat$Test,Stock=Bdat$Stock),pfunc)
Cagg = aggregate(Bdat$Crel,list(MP=Bdat$MP,Test=Bdat$Test,Stock=Bdat$Stock),pfunc)
B50 = C50 = rep(NA,nrow(Bdat))
for(i in 1:nrow(Bdat)){
  ind = Bdat$MP[i] == Bagg[,1] & Bdat$Test[i]==Bagg[,2] & Bdat$Stock[i] == Bagg[,3]
  B50[i] = Bagg[ind,4]
  C50[i] = Cagg[ind,4]
}

Bdat = cbind(Bdat,B50, C50)
levels(Bdat[,3])


# make and order factors for ggplot

for(i in 1:4) Bdat[,i] = as.factor(Bdat[,i])
#Bdat[,2] = factor(Bdat[,2], levels = c("ITC","IRC","ITE", "IRE", "SzMat", "SpC","SP_4010", "SP_MSY"))
#Bdat[,3] = factor(Bdat[,3], levels = c("Ref","Rec_Ext", "Rec_Mod",  "M_Ext",   "M_Mod",  "K_Ext",   "K_Mod", "CF_Ext",  "CF_Mod" ))


saveRDS(Bdat,"Results/Bdat.rds")


# === End of script ============================================================



