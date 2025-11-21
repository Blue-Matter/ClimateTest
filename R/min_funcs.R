
min_cur_SSB = function(MSE_list, horizon = 25, MPno = 1, verbose = T){
  if(class(MSE_list[[1]])=="MSE"){
    Bm = sapply(MSE_list,function(X){X@SSB[,1,horizon]/X@SSB_hist[,X@nyears]})

  }else{ # multihist must be 2-sex in this version
    Bm = sapply(MSE_list, function(X){
      Femind = match("Female",names(X@multiHist))
      nyears = dim(X@multiHist[[Femind]][[1]]@TSdata$SBiomass)[2]
      SSBhist = apply(X@multiHist[[Femind]][[1]]@TSdata$SBiomass[,nyears,],1,sum)
      SSB = X@SSB[,Femind,1,horizon]
      mean(SSB/SSBhist)})


  }
  Bm = mean(Bm)
  if(verbose)cat(paste0("Target ratio = ",round(Bm,2),"\n"))
  (Bm - 1)^2 # stable biomass on average
}
