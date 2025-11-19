
getmult = function(perc,MGT)  (1+(perc/100))^(1/MGT)

# percs = seq(0,18,length.out=7); proyears = 50; OM = readRDS("OMs/Performance/BSH.rds"); OM_list=list(OM); MGT = seq(8,12,length.out=OM@nsim)
getincmat = function(percs,proyears, MGT){
  sapply(percs,function(X,MGT)getmult(X,MGT),MGT=MGT)  # [nsim, perc]annual multiplier required to get per change by MGT
}

# valOM = val_list[[2]]
make_mult_array = function(OMv,inc,increasing=T){
  np = OMv@OM@proyears
  ny = OMv@OM@nyears
  na = OMv@OM@maxage+1
  nsim = OMv@OM@nsim
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
  c(nsim = nsim, nyears = nyears, proyears = proyears, maxage = maxage)
}

# instantaneous natural mortality rate
doM = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  multarray = make_mult_array(OMv,inc)
  yind=OMv@OM@nyears+(1:OMv@OM@proyears)
  OMv@SampPars$Stock$M_ageArray[,,yind] = OMv@SampPars$Stock$M_ageArray[,,yind] * aperm(array(multarray,c(OMv@OM@nsim,OMv@OM@proyears,OMv@OM@maxage+1)),c(1,3,2))
  #matplot(t(OMv@cpars$M_ageArray[1:3,1,]),type="l")
  OMv
}

# Mean recruitment strength
doR = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  multarray = make_mult_array(OMv,inc,increasing=F)
  yind=OMv@OM@maxage+OMv@OM@nyears+(1:OMv@OM@proyears)
  OMv@SampPars$Stock$Perr_y[,yind] = OMv@SampPars$Stock$Perr_y[,yind] * multarray
  OMv
}

# Somatic growth
doK = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  old = OMv@SampPars$Stock$Wt_age

  multarray = make_mult_array(OMv,inc,increasing=F)
  K = array(OMv@SampPars$Stock$K, c(OMv@OM@nsim, OMv@OM@proyears))
  Karr = aperm(array(K*multarray,c(OMv@OM@nsim,OMv@OM@proyears,OMv@OM@maxage+1)),c(1,3,2))
  Linf = array(OMv@SampPars$Stock$Linf, c(OMv@OM@nsim, OMv@OM@proyears))
  Linfarr = aperm(array(Linf,c(OMv@OM@nsim,OMv@OM@proyears,OMv@OM@maxage+1)),c(1,3,2))
  t0 = array(OMv@SampPars$Stock$t0, c(OMv@OM@nsim, OMv@OM@proyears))
  t0arr = aperm(array(t0,c(OMv@OM@nsim,OMv@OM@proyears,OMv@OM@maxage+1)),c(1,3,2))

  agearray = aperm(array((0:OMv@OM@maxage),c(OMv@OM@maxage+1,OMv@OM@nsim, OMv@OM@proyears)),c(2,1,3))
  pro_len_age = Linfarr*(1-exp(-Karr*(agearray+t0arr)))
  yind = OMv@OM@nyears+(1:OMv@OM@proyears)
  OMv@SampPars$Stock$Len_age[,,yind] = pro_len_age

  OMv@SampPars$Stock$Wt_age =  OMv@OM@a *OMv@SampPars$Stock$Len_age ^ OMv@OM@b
  rat = OMv@SampPars$Stock$Wt_age  / old
  OMv@SampPars$Fleet$Wt_age_C =  OMv@SampPars$Fleet$Wt_age_C * rat

  OMv
}

# Spatially-driven increase in catchability
doS = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  Ierr = OMv@SampPars$Obs$Ierr_y #MSEtool::SampleObsPars(OMv)$Ierr_y
  multarray = make_mult_array(OMv,inc,increasing=T)
  yind=OMv@OM@nyears+(1:OMv@OM@proyears)
  Ierr[,yind]=Ierr[,yind]*multarray
  OMv@SampPars$Obs$Ierr_y = Ierr
  OMv@SampPars$Fleet$qinc[] = (inc-1)*100
  OMv
}

# Condition Factor

doC_int = function(inc,OMv){
  dm = get_dim(OMv)
  multarray = make_mult_array(OMv,inc,increasing=F)
  multarray2 = aperm(array(multarray,c(dm$nsim, dm$proyears, dm$maxage+1)),c(1,3,2))
  yind=dm$nyears+(1:dm$proyears)
  OMv@SampPars$Stock$Wt_age[,,yind] =  multarray2 * OMv@SampPars$Stock$Wt_age[,,yind]
  OMv@SampPars$Fleet$Wt_age_C[,,yind] =  multarray2 * OMv@SampPars$Fleet$Wt_age_C[,,yind]
  OMv
}

doC = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doC_int(inc, OMv)
}

doC_MOM = function(X, incmat,val_list){ # need to come back to this when the OM@nsim etc dimensions are sent to multiHist
  inc = incmat[,X]
  multiHist = val_list[[X]]
  nstock = length(multiHist)
  fleet = 1
  for(ss in 1:nstock){
    OMv =  multiHist[[ss]][[fleet]]
    multiHist[[ss]][[fleet]] = doC_int(inc,OMv)
  }
}

do_all = function(Hist){

  if(class(Hist) == 'Hist'){
    incmat = array(1,c(Hist@OM@nsim,1))
    Hist1 = doC(1,incmat,list(Hist))
    Hist2 = doS(1,incmat,list(Hist1))
    Hist3 = doM(1,incmat,list(Hist2))
    Hist4 = doK(1,incmat,list(Hist3))
    Hist5 = doR(1,incmat,list(Hist4))
  }else{
    incmat = array(1,c(Hist[[1]][[1]]@OM@nsim,1))
    Hist1 = doC_MOM(1,incmat,list(Hist))
    Hist2 = doS_MOM(1,incmat,list(Hist1))
    Hist3 = doM_MOM(1,incmat,list(Hist2))
    Hist4 = doK_MOM(1,incmat,list(Hist3))
    Hist5 = doR_MOM(1,incmat,list(Hist4))
  }
  Hist5
}



OM_mod = function(Hist_list, test, percs, horizon){
  #MSE_list = lapply(OM_list,function(X)runMSE(X,Hist=T))
  ni = length(percs)
  out=list()
  for(i in 1:length(Hist_list)){
    MGT = rep(horizon, Hist_list[[i]]@OM@nsim) # floor(MSE_list[[i]]@OMPars$MGT)
    incmat = getincmat(percs,proyears,MGT)   # annual multiplier by sim and perc
    val_list = rep(list(Hist_list[[i]]),ni)
    val_list2 = list()
    for(X in 1:ni)  val_list2[[X]] = do.call(paste0("do",test),args=list(X=X,incmat=incmat,val_list=val_list))

    out[[i]]= val_list2
  }
  out
}


# OM_list = list(OM); MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t"); type ="M"; maxperc=18; horizon = 20; nval = 7; parallel = T




