
# valOM = val_list[[2]]
make_mult_array_v2 = function(OMv,inc,dm,increasing=T){
  nsim=dm$nsim
  np=dm$proyears
  if(increasing)multarray = array(rep(inc,np)^rep(1:np,each=nsim),c(nsim,np))
  if(!increasing)multarray = array(rep(1/inc,np)^rep(1:np,each=nsim),c(nsim,np))
  multarray
}

get_dim_v2 = function(OMv){
  data.frame(nsim = nSim(OMv), nyears = nYear(OMv), proyears = length(Years(OMv))-nYear(OMv), maxage = unlist(nAge(OMv)), nfleet = nFleet(OMv))
}

doM_int_v2 = function(inc, OMv){
  dm = get_dim_v2(OMv)
  yind=dm$nyears+(1:dm$proyears)
  Ma = Extend(OMv@OM@Stock[[1]]@NaturalMortality@MeanAtAge, Years = Years(OMv))
  if(dim(Ma)[1]==1)dm$nsim = 1
  multarray = make_mult_array_v2(OMv,inc,dm,increasing=T)
  multarray2 = aperm(array(multarray,c(dm$nsim, dm$proyears, dm$maxage)),c(1,3,2))
  Ma[,,yind] =  Ma[,,yind,drop=F] * multarray2
  OMv@OM@Stock[[1]]@NaturalMortality@MeanAtAge = Ma
  OMv
}

# instantaneous natural mortality rate
doM_v2 = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  doM_int_v2(inc, OMv)
}


doR_int_v2 = function(inc, OMv){
  dm = get_dim_v2(OMv)
  multarray = make_mult_array_v2(OMv,inc,dm,increasing=F)
  RDproj = OMv@OM@Stock[[1]]@SRR@RecDevProj
  OMv@OM@Stock[[1]]@SRR@RecDevProj = OMv@OM@Stock[[1]]@SRR@RecDevProj * multarray
  OMv
}

# Mean recruitment strength
doR_v2 = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  doR_int_v2(inc, OMv)
}

doK_int_v2= function(inc,OMv,plot=T){
  dm = get_dim_v2(OMv)

  # ---- Calculate length parameters and impose changing K (currently 1 sim only) ------------

  la = OMv@OM@Stock[[1]]@Length@MeanAtAge[,,dm$nyears] # length at age in final year

  # Initial guess of parameters
  Linf0 = max(la)
  K0 = (la[2]/Linf0) * 1.2
  t0 = la[1]

  VBint = function(pars, la, mode = "opt"){ # internal optimization of growth
    Linf = exp(pars[1]); K = exp(pars[2])/Linf; t0 = pars[3]
    age = (1:length(la))-0.5
    lpred = Linf * (1-exp(-K*(age-t0)))
    if(mode=="opt")return(sum((lpred-la)^2))
    if(mode=="pred")return(lpred)
  }

  opt = optim(c(log(Linf0),log(K0*Linf0),t0),VBint,method = "L-BFGS-B",
              lower = c(log(Linf0*0.95),log(K0*Linf0*0.75),-2),
              upper = c(log(Linf0*1.2),log(K0*Linf0*1.5), 2), hessian=T, la= la)

  Linf = exp(opt$par[1])
  K = exp(opt$par[2])/Linf
  t0 = opt$par[3]

  old = OMv@OM@Stock[[1]]@Weight@MeanAtAge
  if(dim(old)[1]==1)dm$nsim = 1

  multarray = make_mult_array_v2(OMv,inc,dm,increasing=F)

  Karr = aperm(array(K*multarray,c(dm$nsim,dm$proyears,dm$maxage)),c(1,3,2))
  Linfarr = aperm(array(Linf,c(dm$nsim,dm$proyears,dm$maxage)),c(1,3,2))
  t0arr = aperm(array(t0,c(dm$nsim,dm$proyears,dm$maxage)),c(1,3,2))

  agearray = aperm(array((1:dm$maxage)-0.5,c(dm$maxage,dm$nsim, dm$proyears)),c(2,1,3))
  pro_len_age = Linfarr*(1-exp(-Karr*(agearray-t0arr)))
  yind = dm$nyears+(1:dm$proyears)

  Len = Extend(Stock@Length@MeanAtAge, Years = Years(OMv)) #Stock@Weight@MeanAtAge
  Len[,,yind] = pro_len_age
  OMv@OM@Stock[[1]]@Length@MeanAtAge = Len


  # ---- Calculate length - weight parameters and calculate new weight (currently 1 sim only)
  wa = OMv@OM@Stock[[1]]@Weight@MeanAtAge[,,dm$nyears] # length at age in final year

  LWint = function(pars,la, wa, mode = "opt"){
    a = exp(pars[1])
    b = exp(pars[2])/a
    wpred = a*la^b
    if(mode == "opt")return(sum((wpred-wa)^2))
    if(mode == "pred")return(wpred)
  }

  b0 = 3
  a0 = wa[length(wa)] / (la[length(la)]^b0)

  opt2 = optim(c(log(a0),log(a0*b0)), LWint, method = "L-BFGS-B",
               lower = c(log(a0*0.5),log(a0*b0*0.5)),
               upper = c(log(a0*1.5),log(a0*b0*1.5)), hessian=T, la=la, wa=wa,mode="opt")

  a = exp(opt2$par[1])
  b = exp(opt2$par[2])/a

  oldwt = newwt = Extend(OMv@OM@Stock[[1]]@Weight@MeanAtAge, Years = Years(OMv))
  newwt[,,yind] = a*OMv@OM@Stock[[1]]@Length@MeanAtAge[,,yind]^b # take new lengths and apply weight at length from last year
  multfac = newwt/oldwt # this is the mulitiplier for all weight matrices

  # Stock weight
  OMv@OM@Stock[[1]]@Weight@MeanAtAge = Extend(OMv@OM@Stock[[1]]@Weight@MeanAtAge, Years=Years(OMv))
  OMv@OM@Stock[[1]]@Weight@MeanAtAge[,,yind] = OMv@OM@Stock[[1]]@Weight@MeanAtAge[,,yind] * multfac[,,yind]

  # Fecundity
  OMv@OM@Stock[[1]]@Fecundity@MeanAtAge = Extend(OMv@OM@Stock[[1]]@Fecundity@MeanAtAge, Years= Years(OMv))
  OMv@OM@Stock[[1]]@Fecundity@MeanAtAge[,,yind] = OMv@OM@Stock[[1]]@Fecundity@MeanAtAge[,,yind] * multfac[,,yind]

  # Weight in catches
  for(ff in 1:nFleet(OMv)){
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetRetained = Extend(OMv@OM@Fleet[[1]][[ff]]@WeightFleetRetained, Years = Years(OMv))
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetRetained[,,yind] = OMv@OM@Fleet[[1]][[ff]]@WeightFleetRetained[,,yind] * multfac[,,yind]
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetSelected = Extend( OMv@OM@Fleet[[1]][[ff]]@WeightFleetSelected, Years = Years(OMv))
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetSelected[,,yind] =  OMv@OM@Fleet[[1]][[ff]]@WeightFleetSelected[,,yind] * multfac[,,yind]
  }

  if(plot){
    par(mfrow=c(1,2), mai=c(0.9,0.9,0.05,0.05))
    matplot(cbind(la,VBint(opt$par,la,mode="pred")), xlab = "Age class", ylab ="Length", col=c("black","red"), type=c("p","l"), pch=19,lty=1)
    matplot(la, cbind(wa,LWint(opt2$par,la,wa,mode="pred")), xlab = "Length", ylab="Weight",col=c("black","red"), type=c("p","l"), pch=19,lty=1)
  }

  OMv
}

# Somatic growth
doK_v2 = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doK_int_v2(inc, OMv)
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

doS_int_v2 = function(inc, OMv){
  dm = get_dim_v2(OMv)
  multarray = make_mult_array_v2(OMv,inc,dm,increasing=T)
  yind=dm$nyears+(1:dm$proyears)
  for(ff in 1:dm$nfleet){
    Ierr = OMv@OM@Obs[[1]][[ff]]@CPUE@Error
    Ierr[,yind]=Ierr[,yind]*multarray
    OMv@OM@Obs[[1]][[ff]]@CPUE@Error = Ierr
    OMv@OM@Fleet[[1]][[ff]]@Catchability@qInc = (inc-1)*100
  }
  OMv
}

# Spatially-driven increase in catchability
doS_v2 = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doS_int_v2(inc, OMv)
}


# Condition Factor

doC_int_v2 = function(inc, OMv){

  Stock = OMv@OM@Stock[[1]]

  # Weight at age
  dm = get_dim_v2(OMv)
  WA = Extend(Stock@Weight@MeanAtAge, Years = Years(OMv)) #Stock@Weight@MeanAtAge
  if(dim(WA)[1]==1)dm$nsim=1
  multarray = make_mult_array_v2(OMv,inc,dm,increasing=F)
  multarray2 = aperm(array(multarray,c(dm$nsim, dm$proyears, dm$maxage)),c(1,3,2))
  yind = dm$nyears+1:dm$proyears
  WA[,,yind] =  WA[,,yind,drop=F] * multarray2
  OMv@OM@Stock[[1]]@Weight@MeanAtAge = WA

  # Fecundity at age
  dm = get_dim_v2(OMv)
  Fec = Extend(Stock@Fecundity@MeanAtAge, Years = Years(OMv))
  if(dim(Fec)[1]==1)dm$nsim=1
  multarray = make_mult_array_v2(OMv,inc,dm,increasing=F)
  multarray2 = aperm(array(multarray,c(dm$nsim, dm$proyears, dm$maxage)),c(1,3,2))
  Fec[,,yind] =  Fec[,,yind,drop=F] * multarray2
  OMv@OM@Stock[[1]]@Fecundity@MeanAtAge = Fec

  # Weight in catches
  for(ff in 1:nFleet(OMv)){
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetRetained = WA
    OMv@OM@Fleet[[1]][[ff]]@WeightFleetSelected = WA
  }

  OMv
}

doC_v2 = function(X, incmat, val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  doC_int_v2(inc, OMv)
}


do_all_v2 = function(hist){

  incmat = array(1,c(nSim(hist),1))
  hist1 = doC_v2(1,incmat,list(hist))
  hist2 = doS_v2(1,incmat,list(hist1))
  hist3 = doM_v2(1,incmat,list(hist2))
  hist4 = doK_v2(1,incmat,list(hist3))
  hist5 = doR_v2(1,incmat,list(hist4))
  hist5
}

OM_mod_v2 = function(Hist_list, test, percs, horizon){
  #MSE_list = lapply(OM_list,function(X)runMSE(X,Hist=T))
  ni = length(percs)
  out=list()
  nsim = nSim(Hist_list[[1]])
  for(i in 1:length(Hist_list)){
    MGT = rep(horizon, nsim) # floor(MSE_list[[i]]@OMPars$MGT)
    incmat = getincmat(percs,proyears,MGT)   # annual multiplier by sim and perc
    val_list = rep(list(Hist_list[[i]]),ni)
    val_list2 = list()
    for(X in 1:ni)  val_list2[[X]] = do.call(paste0("do",test,"_v2"),args=list(X=X,incmat=incmat,val_list=val_list))
    out[[i]]= val_list2
  }
  out
}



