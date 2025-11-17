
CT_perf = function(Hist_list, MPs,test = "M", percs, horizon=20, parallel = T){

  nOM = length(Hist_list)  # number of operating models
  nval = length(percs)   # number of percentage changes to test
  OMs = OM_mod(Hist_list, test, percs, horizon) # a nested list of OMs: OMs[[nOM]][[nval]]
  MSEs = list()

  # for each OM, run the nval scenarios
  for(i in 1:nOM) MSEs[[i]] = sfLapply(OMs[[i]],function(X,MPs){Project(X,MPs)},MPs=MPs)

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



CT_test = function(Hist_list, MPs_tuned, ninc = 5, horizon = 20,
                 tests = c(M = 15, R = 40, K = 25, S = 180, C = 60)){

  ntests = length(tests)
  nMP = length(MPs_tuned)
  MPs = names(MPs_tuned)

  for(mp in 1:nMP)assign(MPs[mp], MPs_tuned[[mp]])
  sfExport(list = MPs)

  Bmetric = Ymetric = list()
  MSEs = list()
  levlist = array(NA,c(ntests,ninc))

  for(tt in 1:ntests){
    test = names(tests)[tt]
    percs = seq(0,tests[tt],length.out = ninc)
    levlist[tt, ] = percs
    MSEs[[tt]] = CT_perf(Hist_list, MPs, test, percs, horizon, parallel = T) # a list of MSE objects ninc long
    cat(paste0(test, " marginal climate test completed (",tt,"/",ntests,") \n"))
  }
  rownames(levlist) = names(tests)

  names(MSEs) = names(tests)
  list(MSEs=MSEs, levlist=levlist)

}

