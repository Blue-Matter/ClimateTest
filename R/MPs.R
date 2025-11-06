

Ir = function (x, Data, reps = 1, targ = 1, nyrs = 3, maxchng = 0.2, maxTACrat = 10, calib_yrs = 5) {
  ind = match(Data@LHYear, Data@Year) - (calib_yrs-1):0
  maxTAC = mean(Data@Cat[x, ind])*maxTACrat
  CpI = mean(Data@Cat[x, ind])/mean(Data@Ind[x, ind], na.rm = T)
  I = Data@Ind[x, ]
  recI = mean(I[length(I) - ((nyrs - 1):0)])
  PropTAC = recI * CpI * targ
  mod = PropTAC/Data@MPrec[x]
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}
class(Ir) = "MP"


It = function (x, Data, reps = 1, targ = 1, nyrs = 3, maxchng = 0.2, maxTACrat = 10, calib_yrs = 5) {
  hind = match(Data@LHYear, Data@Year) - (calib_yrs-1):0
  nowind = length(Data@Ind[x,])- ((nyrs - 1):0)
  maxTAC = mean(Data@Cat[x, hind])*maxTACrat
  Ihist = mean(Data@Ind[x, hind], na.rm = T)
  Inow = mean(Data@Ind[x, nowind], na.rm = T)
  mod = Inow/(Ihist*targ)
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}
class(It) = "MP"
