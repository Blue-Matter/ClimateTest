

SSBrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@SSB[,,horizon] / MSEobj@SSB_hist[,MSEobj@nyears],2,mean)
YDrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@Catch[,,horizon] / MSEobj@CB_hist[,MSEobj@nyears],2,mean)


CT_metrics = function(data, horizon=20, lev_rnd=3){

  test_names = rownames(data$levlist)
  Bmetric = Ymetric = list()
  formals(SSBrel)$horizon = formals(YDrel)$horizon = horizon
  for(tt in 1:length(data$MSEs)){
    MSEtemp = data$MSEs[[tt]]
    MPnames = MSEtemp[[1]]@MPs
    Bmetric[[tt]] = sapply(MSEtemp,function(X)SSBrel(X))
    Ymetric[[tt]] = sapply(MSEtemp,function(X)YDrel(X))
    rownames(Bmetric[[tt]]) = rownames(Ymetric[[tt]]) = MPnames
    colnames(Bmetric[[tt]]) = colnames(Ymetric[[tt]]) = round(data$levlist[tt,],lev_rnd)
  }
  names(Bmetric) = names(Ymetric) = test_names
  list(SSB_relative=Bmetric,Yd_relative=Ymetric)

}

