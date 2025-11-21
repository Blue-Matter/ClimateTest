
getmult = function(perc,MGT)  (1+(perc/100))^(1/MGT)

# percs = seq(0,18,length.out=7); proyears = 50; OM = readRDS("OMs/Performance/BSH.rds"); OM_list=list(OM); MGT = seq(8,12,length.out=OM@nsim)
getincmat = function(percs,proyears, MGT){
  sapply(percs,function(X,MGT)getmult(X,MGT),MGT=MGT)  # [nsim, perc]annual multiplier required to get per change by MGT
}

# valOM = val_list[[2]]
make_mult_array = function(OMv,inc,increasing=T){
  dm = get_dim(OMv)
  np = dm$proyears
  ny = dm$nyears
  na = dm$maxage+1
  nsim = dm$nsim
  if(increasing)multarray = array(rep(inc,np)^rep(1:np,each=nsim),c(nsim,np))
  if(!increasing)multarray = array(rep(1/inc,np)^rep(1:np,each=nsim),c(nsim,np))
  multarray
}

get_dim = function(OMv){
  if(length(OMv@OM@nsim) ==1){ # OM
    nsim = OMv@OM@nsim
    nyears = OMv@OM@nyears
    proyears = OMv@OM@proyears
    maxage = OMv@OM@maxage
  }else{                      #MOM
    nsim = OMv@Misc$MOM@nsim
    nyears = OMv@Misc$MOM@Fleets[[1]][[1]]@nyears
    proyears = OMv@Misc$MOM@proyears
    maxage = OMv@Misc$MOM@Stocks[[1]]@maxage
  }
  data.frame(nsim = nsim, nyears = nyears, proyears = proyears, maxage = maxage)
}

doM_int = function(inc, OMv){
  dm = get_dim(OMv)
  multarray = make_mult_array(OMv,inc)
  yind=dm$nyears+(1:dm$proyears)
  OMv@SampPars$Stock$M_ageArray[,,yind] = OMv@SampPars$Stock$M_ageArray[,,yind] * aperm(array(multarray,c(dm$nsim,dm$proyears,dm$maxage+1)),c(1,3,2))
  OMv
}

# instantaneous natural mortality rate
doM = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  doM_int(inc, OMv)
}

doM_MOM = function(X, incmat, val_list){
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doM_int(inc,OMv)
  }
  multiHist
}


doR_int = function(inc, OMv){
  dm = get_dim(OMv)
  multarray = make_mult_array(OMv,inc,increasing=F)
  yind=dm$maxage+dm$nyears+(1:dm$proyears)
  OMv@SampPars$Stock$Perr_y[,yind] = OMv@SampPars$Stock$Perr_y[,yind] * multarray
  OMv
}
# Mean recruitment strength
doR = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  doR_int(inc, OMv)
}

doR_MOM = function(X, incmat, val_list){
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doR_int(inc,OMv)
  }
  multiHist
}

doK_int = function(inc,OMv){
  dm = get_dim(OMv)

  old = OMv@SampPars$Stock$Wt_age
  multarray = make_mult_array(OMv,inc,increasing=F)
  K = array(OMv@SampPars$Stock$K, c(dm$nsim, dm$proyears))
  Karr = aperm(array(K*multarray,c(dm$nsim,dm$proyears,dm$maxage+1)),c(1,3,2))
  Linf = array(OMv@SampPars$Stock$Linf, c(dm$nsim, dm$proyears))
  Linfarr = aperm(array(Linf,c(dm$nsim,dm$proyears,dm$maxage+1)),c(1,3,2))
  t0 = array(OMv@SampPars$Stock$t0, c(dm$nsim, dm$proyears))
  t0arr = aperm(array(t0,c(dm$nsim,dm$proyears,dm$maxage+1)),c(1,3,2))

  agearray = aperm(array((0:dm$maxage),c(dm$maxage+1,dm$nsim, dm$proyears)),c(2,1,3))
  pro_len_age = Linfarr*(1-exp(-Karr*(agearray+t0arr)))
  yind = dm$nyears+(1:dm$proyears)
  OMv@SampPars$Stock$Len_age[,,yind] = pro_len_age

  OMv@SampPars$Stock$Wt_age =  OMv@SampPars$Stock$a * OMv@SampPars$Stock$Len_age ^ OMv@SampPars$Stock$b
  rat = OMv@SampPars$Stock$Wt_age  / old
  OMv@SampPars$Fleet$Wt_age_C =  OMv@SampPars$Fleet$Wt_age_C * rat

  OMv
}
# Somatic growth
doK = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doK_int(inc, OMv)
}

doK_MOM = function(X, incmat, val_list){
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doK_int(inc,OMv)
  }
  multiHist
}


doS_int = function(inc, OMv){
  dm = get_dim(OMv)
  Ierr = OMv@SampPars$Obs$Ierr_y #MSEtool::SampleObsPars(OMv)$Ierr_y
  multarray = make_mult_array(OMv,inc,increasing=T)
  yind=dm$nyears+(1:dm$proyears)
  Ierr[,yind]=Ierr[,yind]*multarray
  OMv@SampPars$Obs$Ierr_y = Ierr
  OMv@SampPars$Fleet$qinc[] = (inc-1)*100
  OMv
}

# Spatially-driven increase in catchability
doS = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doS_int(inc, OMv)
}

doS_MOM = function(X, incmat, val_list){
  # !!! Note that you need to alter AddInd_Err and qinc by fleet
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doS_int(inc,OMv)
  }
  multiHist
}

# Condition Factor

doC_int = function(inc, OMv){
  dm = get_dim(OMv)
  multarray = make_mult_array(OMv,inc,increasing=F)
  multarray2 = aperm(array(multarray,c(dm$nsim, dm$proyears, dm$maxage+1)),c(1,3,2))
  yind=dm$nyears+(1:dm$proyears)
  OMv@SampPars$Stock$Wt_age[,,yind] =  multarray2 * OMv@SampPars$Stock$Wt_age[,,yind]
  OMv@SampPars$Fleet$Wt_age_C[,,yind] =  multarray2 * OMv@SampPars$Fleet$Wt_age_C[,,yind]
  OMv
}

doC = function(X, incmat, val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doC_int(inc, OMv)
}

doC_MOM = function(X, incmat, val_list){
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doC_int(inc,OMv)
  }
  multiHist
}

do_all = function(Hist){

  if(any(class(Hist) == 'Hist')){
    incmat = array(1,c(Hist@OM@nsim,1))
    Hist1 = doC(1,incmat,list(Hist))
    Hist2 = doS(1,incmat,list(Hist1))
    Hist3 = doM(1,incmat,list(Hist2))
    Hist4 = doK(1,incmat,list(Hist3))
    Hist5 = doR(1,incmat,list(Hist4))
  }else{
    dm = get_dim(Hist[[1]][[1]])
    incmat = array(1,c(dm$nsim,1))
    Hist1 = doC_MOM(1,incmat,list(Hist))
    Hist2 = doS_MOM(1,incmat,list(Hist1))
    Hist3 = doM_MOM(1,incmat,list(Hist2))
    Hist4 = doK_MOM(1,incmat,val_list = list(Hist3))
    Hist5 = doR_MOM(1,incmat,list(Hist4))
  }
  Hist5
}

# dim(Hist5$Female$`Fleet 1`@SampPars$Stock$Wt_age)

OM_mod = function(Hist_list, test, percs, horizon){
  #MSE_list = lapply(OM_list,function(X)runMSE(X,Hist=T))
  ni = length(percs)
  out=list()
  if(any(class(Hist_list[[1]]) == 'Hist')) nsim = Hist@OM@nsim
  if(any(class(Hist_list[[1]]) == 'multiHist')) nsim = get_dim(Hist_list[[1]][[1]][[1]])$nsim
  for(i in 1:length(Hist_list)){
    MGT = rep(horizon, nsim) # floor(MSE_list[[i]]@OMPars$MGT)
    incmat = getincmat(percs,proyears,MGT)   # annual multiplier by sim and perc
    val_list = rep(list(Hist_list[[i]]),ni)
    val_list2 = list()

    if(any("multiHist"%in%class(Hist_list[[1]]))){
      for(X in 1:ni)  val_list2[[X]] = do.call(paste0("do",test,"_MOM"),args=list(X=X,incmat=incmat,val_list=val_list))
    }else{
      for(X in 1:ni)  val_list2[[X]] = do.call(paste0("do",test),args=list(X=X,incmat=incmat,val_list=val_list))
    }
    out[[i]]= val_list2
  }
  out
}


# OM_list = list(OM); MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t"); type ="M"; maxperc=18; horizon = 20; nval = 7; parallel = T




