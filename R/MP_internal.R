# calculates a TAC from a TAC modifier, maximum TAC changes and maxTAC
doRec = function(MPrec, mod, maxchng, maxTAC){ 
  if(mod > (1+maxchng))mod = 1+maxchng
  if(mod < (1-maxchng))mod = 1-maxchng
  Rec = new('Rec')
  Rec@TAC = min(MPrec*mod, maxTAC)
  Rec
}