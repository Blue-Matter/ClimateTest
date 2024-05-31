# === Run closed-loop simulations ==============================================

# Tom Carruthers 
# May 2024

library(openMSE)
CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)
source("Code/02_make_MPs.R")
source("Code/03_make_tests.R")



# --- Define runs --------------------------------------------------------------

Snames = c("BET","SWO","BSH","SMA","WHM","BUM")
OM_files = paste0("OMs/OM_",Snames,".rds")
ns = length(Snames)

modname = c("Ref",paste(rep(c("Mod.","Ext."),each=4),rep(c("M","CF","Rec","K")))
modtype = c("No_ch","M_ch","M_ch","CF_ch","CF_ch","Rec_ch","Rec_ch","K_ch","K_ch")
bys =     c(0,       20,    -10,    -20,    -20,     -40,     -10,   -20)
nm = length(modtype)

MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat")

# --- Run simulations ----------------------------------------------------------


for(ss in 1:ns){
  
  OM = readRDS(OM_files[[ss]])
  
  for(mm in 1:nm){
    cat(paste0("Running ", Snames[ss],"_",modname[mm],"\n"))
    OM_mod = do.call(modtype[mm],args=list(OM=OM,by = bys[mm]))
    MSE = runMSE(OM_mod,MPs)
    MSE@Name = paste0(Snames[ss],"_",modname[mm])
    saveRDS(MSE,file = paste0("MSEs/",Snames[ss],"_",modname[mm],".rds"))
    
  }
  
}




