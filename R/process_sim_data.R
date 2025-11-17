

CT_Metrics = function(CT_data, lev_rnd=3){

  test_names = names(CT_data)
  Bmetric = Ymetric = list()
  for(tt in 1:length(CT_data$MSEs)){
    MSEtemp = CT_data$MSEs[[tt]]
    MPnames = MSEtemp[[1]]@MPs
    Bmetric[[tt]] = sapply(MSEtemp,function(X)Brel(X)@Mean)
    Ymetric[[tt]] = sapply(MSEtemp,function(X)Yrel(X)@Mean)
    rownames(Bmetric[[tt]]) = rownames(Ymetric[[tt]]) = MPnames
    colnames(Bmetric[[tt]]) = colnames(Ymetric[[tt]]) = round(CT_data$levlist[tt,],lev_rnd)
  }
  names(Bmetric) = names(Ymetric) = names(tests)
  list(Bmetric=Bmetric,Ymetric=Ymetric)
}

