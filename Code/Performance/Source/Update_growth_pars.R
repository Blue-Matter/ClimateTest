

findK = function(par,obslen, Linf, t0, type = 'opt'){
  K = exp(par)
  age = 1:length(obslen)
  estlen = Linf*(1-exp(-K * (age-t0)))
  if(type=='opt')return(sum((estlen-obslen)^2))
  if(type=='pred')return(estlen)
}


updateGrowth = function(OM, t0 = 0.8, Linf = 179){
  iK = OM@K[1]
  obslen = OM@cpars$Len_age[1,,OM@nyears]
  opt = optimize(findK, interval =log(c(iK/2,iK*2)), obslen = obslen, Linf=Linf, t0=t0)
  
  plot(findK(opt$minimum,obslen,Linf,t0,type="pred"),col='red',type='l')
  points(obslen,col='black')
  OM@K = rep(exp(opt$minimum),2)
  OM@t0 = rep(t0,2)
  OM@Linf = rep(Linf,2)
  OM
}

abopt = function(pars,Wt_age,Len_age, mode='opt'){
  pred = exp(pars[1])*Len_age^exp(pars[2])
  if(mode=='opt')return(sum((pred - Wt_age)^2))
  if(mode =='pred')return(pred)
}


get_ab = function(OM){
  Wt_age = OM@cpars$Wt_age[1,3:(OM@maxage+1),OM@nyears]
  Len_age = OM@cpars$Len_age[1,3:(OM@maxage+1),OM@nyears]
  binit = OM@b
  ainit = mean( Wt_age / Len_age^OM@b)
  
  opt = optim(log(c(ainit,binit)), abopt, method = "L-BFGS-B", 
              lower = log(c(ainit,binit)/3), upper = log(c(ainit,binit)*2),
              Wt_age=Wt_age, Len_age = Len_age) 
  
  pred = abopt(opt$par,Wt_age, Len_age, mode="pred")
  
  matplot(cbind(Wt_age,pred),col=c('black','red'),type=c("p","l"))
  OM@a = exp(opt$par[1])
  OM@b = exp(opt$par[2])
  OM
}