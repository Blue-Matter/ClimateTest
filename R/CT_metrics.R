

SSBrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@SSB[,,horizon] / MSEobj@SSB_hist[,MSEobj@nyears],2,mean)
YDrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@Catch[,,horizon] / MSEobj@CB_hist[,MSEobj@nyears],2,mean)


CT_metrics = function(CT_data, horizon=20, lev_rnd=3){

  test_names = names(CT_data)
  Bmetric = Ymetric = list()
  formals(SSBrel)$horizon = formals(YDrel)$horizon = horizon
  for(tt in 1:length(CT_data$MSEs)){
    MSEtemp = CT_data$MSEs[[tt]]
    MPnames = MSEtemp[[1]]@MPs
    Bmetric[[tt]] = sapply(MSEtemp,function(X)SSBrel(X))
    Ymetric[[tt]] = sapply(MSEtemp,function(X)YDrel(X))
    rownames(Bmetric[[tt]]) = rownames(Ymetric[[tt]]) = MPnames
    colnames(Bmetric[[tt]]) = colnames(Ymetric[[tt]]) = round(CT_data$levlist[tt,],lev_rnd)
  }
  names(Bmetric) = names(Ymetric) = names(tests)
  list(SSB_relative=Bmetric,Yd_relative=Ymetric)

}

