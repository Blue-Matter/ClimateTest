# === Run closed-loop simulations ==============================================

# Tom Carruthers 
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)
source("Code/02_make_MPs.R")
source("Code/03_make_tests.R")



# --- Define runs --------------------------------------------------------------

Snames = c("BET","SWO","BSH") #Snames = c("BET","SWO","BSH","SMA","WHM","BUM")
OM_files = paste0("OMs/OM_",Snames,".rds")
ns = length(Snames)
avail("ClimateTest")

test_name = c("Ref",paste0(rep(c("M","CF","Rec","K"),each=2),"_",rep(c("Mod","Ext"),4)))
test_type = c("No_ch","M_ch","M_ch","CF_ch","CF_ch","Rec_ch","Rec_ch","K_ch","K_ch")
bys =     c(0,       10,    20,    -10,    -20,    -20,     -40,     -10,   -20)
nt = length(test_type)

MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat","SP_MSY","SP_4010")
# MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat","FMSYref","FMSYref75")

# --- Run simulations ----------------------------------------------------------

runs = expand.grid(1:ns,1:nt)
nams = paste0(Snames[runs[,1]],"_",test_name[runs[,2]],".rds")
runs = runs[!(nams %in% list.files("MSEs_wAssess")),]


dorun = function(x,runs,OM_files,Snames,test_name,MPs,bys){
  ss = runs[x,1]
  tt = runs[x,2]
  OM = readRDS(OM_files[[ss]])
  cat(paste0("Running ", Snames[ss],"_",test_name[tt],"\n"))
  OM_mod = do.call(test_type[tt],args=list(OM=OM,by = bys[tt]))
  MSE = runMSE(OM_mod,MPs,silent=T)
  #MSE@Name = paste0(Snames[ss],"_",test_name[tt])
  saveRDS(MSE,file = paste0("MSEs_wAssess/",Snames[ss],"_",test_name[tt],".rds"))
}

# --- In parallel --------------------------------------------------------------

sfInit(parallel=T,cpus=8)
sfExport('runs')
sfLibrary(openMSE)
sfExportAll()
sfSapply(1:nrow(runs),dorun,runs=runs,OM_files=OM_files,Snames=Snames,test_name=test_name,MPs=MPs,bys=bys)



# --- Non parallel ------------------------------------------------------------- 

for(x in 1:nrow(runs))dorun(x, runs=runs,OM_files=OM_files,Snames=Snames,test_name=test_name,MPs=MPs,bys=bys)

for(ss in 1:ns){
  OM = readRDS(OM_files[[ss]])
  for(tt in 1:nt){
    cat(paste0("Running ", Snames[ss],"_",test_name[tt],"\n"))
    OM_mod = do.call(test_type[tt],args=list(OM=OM,by = bys[tt]))
    MSE = runMSE(OM_mod,MPs,silent=T)
    MSE@Name = paste0(Snames[ss],"_",test_name[tt])
    #Pplot(MSE,nam=MSE@Name)
    saveRDS(MSE,file = paste0("MSEs/",Snames[ss],"_",test_name[tt],".rds"))
  }
}


# === End of Script ============================================================

