
getmult = function(perc,MGT)  (1+(perc/100))^(1/MGT)

# percs = seq(0,18,length.out=7); proyears = 50; OM = readRDS("OMs/Performance/BSH.rds"); OM_list=list(OM); MGT = seq(8,12,length.out=OM@nsim)
getincmat = function(percs,proyears, MGT){
  sapply(percs,function(X,MGT)getmult(X,MGT),MGT=MGT)  # [nsim, perc]annual multiplier required to get per change by MGT
}

# valOM = val_list[[2]]
make_mult_array = function(OMv,inc,increasing=T){
  dims = dim(OMv@cpars$M_ageArray)
  np = OMv@proyears
  ny = OMv@nyears
  na = dims[2]
  nsim = dims[1]
  if(increasing)multarray = array(rep(inc,np)^rep(1:np,each=nsim),c(nsim,np))
  if(!increasing)multarray = array(rep(1/inc,np)^rep(1:np,each=nsim),c(nsim,np))
  multarray
}


doM = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  multarray = make_mult_array(OMv,inc)
  yind=OMv@nyears+(1:OMv@proyears)
  OMv@cpars$M_ageArray[,,yind] = OMv@cpars$M_ageArray[,,yind] * aperm(array(multarray,c(OM@nsim,OM@proyears,OM@maxage+1)),c(1,3,2))
  #matplot(t(OMv@cpars$M_ageArray[1:3,1,]),type="l")
  OMv
}

doR = function(X,incmat,val_list){
  inc = incmat[,X]
  OMv = val_list[[X]]
  multarray = make_mult_array(OMv,inc,increasing=F)
  yind=OM@maxage+OMv@nyears+(1:OMv@proyears)
  OMv@cpars$Perr_y[,yind] = OMv@cpars$Perr_y[,yind] * multarray
  OMv
}



doK = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  if(inc[1]!=1){ # temporary fix
    old = OMv@cpars$Wt_age
    old_C = OMv@cpars$Wt_age_C
    multarray = make_mult_array(OMv,inc,increasing=F)
    Karr = aperm(array(OMv@K[1]*multarray,c(OMv@nsim,OMv@proyears,OMv@maxage+1)),c(1,3,2))
    agearray = aperm(array((0:OMv@maxage)+1,c(OMv@maxage+1,OMv@nsim, OMv@proyears)),c(2,1,3))
    pro_len_age = OMv@Linf[1]*(1-exp(-Karr*(agearray-OM@t0[1])))
    yind = OMv@nyears+(1:OMv@proyears)
    OMv@cpars$Len_age[,,yind] = pro_len_age
    OMv@cpars$Wt_age =  OMv@a * OMv@cpars$Len_age ^ OMv@b
    rat = OMv@cpars$Wt_age / old
    OMv@cpars$Wt_age_C = OMv@cpars$Wt_age_C * rat
  }
  OMv
}

doS = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  Ierr = runMSE(OMv,Hist=T,parallel=T)@SampPars$Obs$Ierr_y
  multarray = make_mult_array(OMv,inc,increasing=T)
  yind=OMv@nyears+(1:OMv@proyears)
  Ierr[,yind]=Ierr[,yind]*multarray
  OMv@cpars$Ierr_y = Ierr
  OMv
}

doC = function(X, incmat,val_list){ # only deterministic currently
  inc = incmat[,X]
  OMv = val_list[[X]]
  multarray = make_mult_array(OMv,inc,increasing=F)
  multarray2 = aperm(array(multarray,c(OMv@nsim, OMv@proyears, OMv@maxage+1)),c(1,3,2))
  yind=OMv@nyears+(1:OMv@proyears)
  OMv@cpars$Wt_age[,,yind] =  multarray2*OMv@cpars$Wt_age[,,yind]
  OMv@cpars$Wt_age_C[,,yind] =  multarray2*OMv@cpars$Wt_age_C[,,yind]
  OMv
}

OM_mod = function(OM_list, type, percs, horizon){
  #MSE_list = lapply(OM_list,function(X)runMSE(X,Hist=T))
  ni = length(percs)
  out=list()
  for(i in 1:length(OM_list)){
    OM = OM_list[[i]]
    MGT = rep(horizon, OM@nsim) #floor(MSE_list[[i]]@OMPars$MGT)
    incmat = getincmat(percs,proyears,MGT) # annual multiplier by sim and perc
    val_list = rep(list(OM),ni)
    val_list2 = list()
    for(X in 1:ni){
      val_list2[[X]] = do.call(paste0("do",type),args=list(X=X,incmat=incmat,val_list=val_list))
    }
    out[[i]]= val_list2
  }
  out
}


# OM_list = list(OM); MPs = paste0(rep(c("It","Ir","Is"),each=2),rep(c("_5","_10"),3),"t"); type ="M"; maxperc=18; horizon = 20; nval = 7; parallel = T

CT_perf = function(OM_list, MPs, type = "M", percs, horizon=20, parallel = T){
 
 nOM = length(OM_list)  # number of operating models
 nval = length(percs)   # number of percentage changes to test
 OMs = OM_mod(OM_list, type, percs, horizon) # a nested list of OMs: OMs[[nOM]][[nval]]
 
 MSEs = list()
 
 # for each OM, run the nval scenarios
 for(i in 1:nOM){
   if(!parallel)MSEs[[i]] = lapply(OMs[[i]],runMSE(X,MPs),MPs=MPs)  # OMs for each value within OM_list object
   if(parallel)MSEs[[i]] = sfLapply(OMs[[i]],function(X,MPs)runMSE(X,MPs),MPs=MPs)
 }

 # join (across nOM) the MSEs into one per nval scenario  
 if(nOM == 1) MSEjoin = MSEs[[1]]
 if(nOM > 1){
   MSEjoin = list()
   for(i in 1:nval){
     MSEobjs = list()
     for(x in 1:nOM)   MSEobjs[[x]] = MSEs[[x]][[i]] #list across x OMs for the same val i
     MSEjoin[[i]] = joinMSE(MSEobjs) # join over OMs for perf calc
   }
 }
 
 MSEjoin # a list of MSEs nval long MSEjoin[[nval]]
  
}


