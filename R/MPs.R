
#' Generic Index Ratio MP for Climate Test Demonstration
#'
#' Sets TAC advice based on current index levels and a multiplier (targ) of last historical catch/Index
#'
#' @param x Simulation number
#' @param Data OpenMSE objects of class 'Data'
#' @param reps Not used
#' @param tune Positive real number, TAC is set by TAC = recent_index x tune x recent_historical_catch/recent_historical_index
#' @param nyrs integer, the number of recent years over which to average the index of abundance.
#' @param maxchng Positive real number, the maximum rate of change in TAC (upward and downward)
#' @param maxTACrat Positive real number, sets the maximum TAC to maxTACrat x mean catches over last historical calib_yrs
#' @param calib_yrs Positive integer the number of last historical years to define maxTACrat and the catch/index
#' @examples
#' Ir(1, Example_datafile)
#' @author T. Carruthers
#' @export
Ir = function (x, Data, reps = 1, tune = 1, nyrs = 3, maxchng = 0.2, maxTACrat = 10, calib_yrs = 5) {
  ind = match(Data@LHYear, Data@Year) - (calib_yrs-1):0
  maxTAC = mean(Data@Cat[x, ind])*maxTACrat
  CpI = mean(Data@Cat[x, ind])/mean(Data@Ind[x, ind], na.rm = T)
  I = Data@Ind[x, ]
  recI = mean(I[length(I) - ((nyrs - 1):0)])
  PropTAC = recI * CpI * tune
  lastrec = Data@MPrec[x]
  if(is.na(lastrec))lastrec = Data@Cat[x,length(Data@Cat[x,])]
  mod = PropTAC/lastrec
  doRec(lastrec, mod, c(0,maxchng),c(0,maxchng),c(0,maxTAC))
}
class(Ir) = "MP"

#' Generic Index Target MP for Climate Test Demonstration
#'
#' Sets TAC advice based on current index levels and a multiplier (targ) of last historical catch/Index
#'
#' @param x Simulation number
#' @param Data OpenMSE objects of class 'Data'
#' @param reps Not used
#' @param tune Positive real number, TAC is set by TAC = lastTAC x Recent_index / (recent_historical_index x tune)
#' @param nyrs integer, the number of recent years over which to average the index of abundance.
#' @param maxchng Positive real number, the maximum rate of change in TAC (upward and downward)
#' @param maxTACrat Positive real number, sets the maximum TAC to maxTACrat x mean catches over last historical calib_yrs
#' @param calib_yrs Positive integer the number of last historical years to define maxTACrat and recent_historical_index
#' @examples
#' It(1, Example_datafile)
#' @author T. Carruthers
#' @export
It = function (x, Data, reps = 1, tune = 1, nyrs = 3, maxchng = 0.2, maxTACrat = 10, calib_yrs = 5) {
  hind = match(Data@LHYear, Data@Year) - (calib_yrs-1):0
  nowind = length(Data@Ind[x,])- ((nyrs - 1):0)
  maxTAC = mean(Data@Cat[x, hind])*maxTACrat
  Ihist = mean(Data@Ind[x, hind], na.rm = T)
  Inow = mean(Data@Ind[x, nowind], na.rm = T)
  mod = Inow/(Ihist * tune)
  lastrec = Data@MPrec[x]
  if(is.na(lastrec))lastrec = Data@Cat[x,length(Data@Cat[x,])]
  doRec(lastrec, mod, c(0,maxchng),c(0,maxchng),c(0,maxTAC))
}
class(It) = "MP"
