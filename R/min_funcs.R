
min_cur_SSB = function(MSE_list, horizon = 20, MPno = 1, verbose = T){
  Bm = sapply(MSE_list,function(X){X@SSB[,1,horizon]/X@SSB_hist[,X@nyears]})
  Bm = mean(Bm)
  if(verbose)cat(paste0("Target ratio = ",round(Bm,2),"\n"))
  (Bm - 1)^2 # stable biomass on average
}
