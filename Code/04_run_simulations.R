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
avail("ClimateTest")

test_name = c("Ref",paste0(rep(c("M","CF","Rec","K"),each=2),"_",rep(c("Mod","Ext"),4)))
test_type = c("No_ch","M_ch","M_ch","CF_ch","CF_ch","Rec_ch","Rec_ch","K_ch","K_ch")
bys =     c(0,       10,    20,    -10,    -20,    -20,     -40,     -10,   -20)
nt = length(test_type)

MPs = c("ITC","IRC","ITE","IRE","SpC","SzMat","SP_MSY","SP_4010")

# --- Run simulations ----------------------------------------------------------


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




