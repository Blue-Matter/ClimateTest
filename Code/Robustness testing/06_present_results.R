
# === Present results ==========================================================

# Tom Carruthers 
# May 2024

library(openMSE)
library(ggplot2)

CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)

Bdat = readRDS("Results/Bdat.rds")

#NAcond = Bdat$Stock == "BET" & grepl("K_",Bdat$Test)
#Bdat$Brel[NAcond] = NA

Bdat_Mod = Bdat[grepl("Mod",Bdat$Test)|grepl("Ref",Bdat$Test),]
Bdat_Ext = Bdat[grepl("Ext",Bdat$Test)|grepl("Ref",Bdat$Test),]


# -------- Biomass -----------------

# by MP

ggplot(Bdat_Ext, aes(x=MP,y=Brel,fill=B50)) + 
  geom_boxplot() + 
  facet_wrap(~Test+Stock,ncol=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low='green',high="red")+
  ylim(0,2.1)

ggsave("Figures/B_ext_MPComp.jpg",height=8,width=10,dpi=400,units='in')


# by Test

ggplot(Bdat_Ext, aes(x=Test,y=Brel,fill=B50)) + 
  geom_boxplot() + 
  facet_wrap(~MP+Stock,ncol=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low='green',high="red")+
  ylim(0,2.1)

ggsave("Figures/B_ext_TestComp.jpg",height=10,width=7,dpi=400,units='in')

# -------- Catch -----------------

# by MP

ggplot(Bdat_Ext, aes(x=MP,y=Crel,fill=C50)) + 
  geom_boxplot() + 
  facet_wrap(~Test+Stock,ncol=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low='green',high="red")+
  ylim(0,2.1)

ggsave("Figures/C_ext_MPComp.jpg",height=8,width=10,dpi=400,units='in')


# by Test

ggplot(Bdat_Ext, aes(x=Test,y=Crel,fill=C50)) + 
  geom_boxplot() + 
  facet_wrap(~MP+Stock,ncol=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low='green',high="red")+
  ylim(0,2.1)

ggsave("Figures/C_ext_TestComp.jpg",height=10,width=7,dpi=400,units='in')




# Trade-off declines


Bmus = aggregate(Bdat$Brel,by=list(MP = Bdat$MP, Stock = Bdat$Stock, Test = Bdat$Test),mean)
Cmus = aggregate(Bdat$Crel,by=list(MP = Bdat$MP, Stock = Bdat$Stock, Test = Bdat$Test),mean)

Bres = Bmus[Bmus$Test != "Ref",]; 
Cres = Cmus[Cmus$Test != "Ref",]; 

Bres = cbind(Bres,rep("CT",nrow(Bres)));names(Bres)[4:5] = c("EB","Run")
Cres = cbind(Cres,rep("CT",nrow(Cres)));names(Cres)[4:5] = c("EC","Run")

Brefs = Bmus[Bmus$Test == "Ref",]
Crefs = Cmus[Cmus$Test == "Ref",]


nTests = length(unique(Bres$Test))

Brefall = cbind(Bres[,1:3],rep(Brefs$x, nTests),rep("Ref",nrow(Bres))); names(Brefall)[4:5] = c("EB","Run")
Crefall = cbind(Cres[,1:3],rep(Crefs$x, nTests),rep("Ref",nrow(Cres))); names(Crefall)[4:5] = c("EC","Run")

EC = c(Crefall$EC,Cres$EC)
TOdat = cbind(rbind(Brefall,Bres),EC)
TOdat$Run = as.factor(TOdat$Run)

TOdat_Mod = TOdat[grepl("Mod",TOdat$Test),]
TOdat_Ext = TOdat[grepl("Ext",TOdat$Test),]


ggplot(TOdat_Ext, aes(x=EB,y=EC,group=MP)) + 
  geom_path(aes(color=MP),arrow = arrow(length=unit(0.15,"cm"), ends="last", type = "closed"))+
  facet_wrap(~Test+Stock,ncol=3)

ggsave("Figures/TO_Ext.jpg",height=9,width=8,dpi=400,units='in')



ggplot(TOdat_Mod, aes(x=EB,y=EC,group=MP)) + 
  geom_path(aes(color=MP),arrow = arrow(length=unit(0.15,"cm"), ends="last", type = "closed"))+
  facet_wrap(~Test+Stock,ncol=3)

ggsave("Figures/TO_Mod_ppt.jpg",height=5.5,width=7.5,dpi=400,units='in')


# === End of script ============================================================




