

SSBrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@SSB[,,horizon] / MSEobj@SSB_hist[,MSEobj@nyears],2,mean)
YDrel = function (MSEobj = NULL, horizon=20)  apply(MSEobj@Catch[,,horizon] / MSEobj@CB_hist[,MSEobj@nyears],2,mean)

SSBrel_multi = function (MSEobj = NULL, horizon=20){
  Femind = match("Female",MSEobj@Snames)
  nyears = dim(MSEobj@multiHist[[Femind]][[1]]@TSdata$SBiomass)[2]
  SSBhist = apply(MSEobj@multiHist[[Femind]][[1]]@TSdata$SBiomass[,nyears,],1,sum)
  SSB = MSEobj@SSB[,Femind,,horizon]
  apply(SSB/SSBhist,2,mean)
}

YDrel_multi = function (MSEobj = NULL, horizon=20){
  Femind = match("Female",MSEobj@Snames)
  nyears = dim(MSEobj@multiHist[[Femind]][[1]]@TSdata$SBiomass)[2]
  YDhist = apply(MSEobj@multiHist[[Femind]][[1]]@TSdata$Removals[,nyears,],1,sum)
  YD = apply(MSEobj@Catch[,,,,horizon,drop=F],c(1,4),sum) # sum over stocks (sexes)
  apply(YD,2,mean)
}


#' Climate Test Metrics summary.
#'
#' Produces a table of yield and biomass performance at the end of a specified time horizon for each test
#'
#' @param horizon Positive integer. The time horizon (number of projected years) at which the outcome (e.g. SSB) is tuned to be the same as current levels.
#' @param lev_rnd Positive integer. The number of significant digits to report the levels of each test.
#' @param dat_rnd Positive integer. The number of significant digits to report the yield and biomass metrics from each test.
#' @return A list of dataframes
#' @examples
#' OM_list = list(BET_1,BET_2)
#' Hist_list = CT_1_prep(OM_list)
#' MPs_tuned = CT_2_tune(Hist_list, c("Ir","It"))
#' CT_data = CT_3_test(Hist_list, MPs_tuned)
#' CT_metrics(CT_data)
#' @author T. Carruthers
#' @export
CT_metrics = function(CT_data, horizon=20, lev_rnd=3, dat_rnd = 5){

  test_names = rownames(CT_data$levlist)
  Bmetric = Ymetric = list()
  formals(SSBrel)$horizon = formals(YDrel)$horizon = formals(SSBrel_multi)$horizon = formals(YDrel_multi)$horizon = horizon
  for(tt in 1:length(CT_data$MSEs)){
    MSEtemp = CT_data$MSEs[[tt]]
    if(any("MSE" %in% class(MSEtemp[[1]]))){
      MPnames = MSEtemp[[1]]@MPs
      Bmetric[[tt]] = round(sapply(MSEtemp,function(X)SSBrel(X)),dat_rnd)
      Ymetric[[tt]] = round(sapply(MSEtemp,function(X)YDrel(X)),dat_rnd)
    }else{
      MPnames = MSEtemp[[1]]@MPs[[1]]
      Bmetric[[tt]] = round(sapply(MSEtemp,function(X)SSBrel_multi(X)),dat_rnd)
      Ymetric[[tt]] = round(sapply(MSEtemp,function(X)YDrel_multi(X)),dat_rnd)
    }
    rownames(Bmetric[[tt]]) = rownames(Ymetric[[tt]]) = MPnames
    colnames(Bmetric[[tt]]) = colnames(Ymetric[[tt]]) = round(CT_data$levlist[tt,],lev_rnd)
  }
  names(Bmetric) = names(Ymetric) = test_names
  list(SSB_relative=Bmetric,Yd_relative=Ymetric)

}

