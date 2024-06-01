
# === Present results ==========================================================

# Tom Carruthers 
# May 2024

library(openMSE)
library(ggplot2)

CTdir = "C:/GitHub/ClimateTest"
setwd(CTdir)

Cdat = readRDS("Results/Cdat.rds")
Bdat = readRDS("Results/Bdat.rds")

NAcond = Cdat$Stock == "BET" & grepl("K_",Cdat$Test)
Cdat$Crel[NAcond] = NA
Bdat$Brel[NAcond] = NA

Bdat_Mod = Bdat[grepl("Mod",Bdat$Test)|grepl("Ref",Bdat$Test),]
Bdat_Ext = Bdat[grepl("Ext",Bdat$Test)|grepl("Ref",Bdat$Test),]

Cdat_Mod = Cdat[grepl("Mod",Cdat$Test)|grepl("Ref",Cdat$Test),]
Cdat_Ext = Cdat[grepl("Ext",Cdat$Test)|grepl("Ref",Cdat$Test),]


ggplot(Bdat_Mod, aes(x=MP,y=Brel)) + 
  geom_boxplot() + 
  facet_wrap(~Test+Stock,ncol=3) # comps by year



ggplot(Bdat_Ext, aes(x=MP,y=Brel,fill=P50)) + 
  geom_boxplot() + 
  facet_wrap(~Test+Stock,ncol=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low='green',high="red")

ggsave("Figures/B_ext_MPComp.jpg",height=9,width=12,dpi=400,units='in')


ggplot(Bdat_Ext, aes(x=Test,y=Brel)) + 
  geom_boxplot() + 
  facet_wrap(~MP+Stock,ncol=3) # comps by year

ggsave("Figures/B_ext_TestComp.jpg",height=11,width=8,dpi=400,units='in')



ggplot(Cdat_Ext, aes(x=Test,y=Crel)) + 
  geom_boxplot() + 
  facet_wrap(~MP+Stock,ncol=3) # comps by year

ggsave("Figures/C_ext_TestComp.jpg",height=11,width=8,dpi=400,units='in')



# === End of script ============================================================




