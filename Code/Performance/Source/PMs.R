
PGK_dyn <- function (MSEobj = NULL, Ref = 1, Yrs = c(1,50))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_dyn: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) (dynamic) in Years 1-50"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (dynamic) (years 1-50)"
  
  PMobj@Ref <- Ref
  tt <- MSEobj@SB_SBMSY > 1 & MSEobj@F_FMSY < 1
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
  
}
class(PGK_dyn) <- 'PM'

PGK_stat <- function (MSEobj = NULL, Ref = 1, Yrs = c(1,50))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_stat: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) (static, year 0) in Years 1-50"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (static, year 0) (years 1-50)"
  
  PMobj@Ref <- Ref
  Brel = MSEobj@SSB / array(MSEobj@RefPoint$SSBMSY[,,MSEobj@nyears],dim(MSEobj@SSB))
  Frel = MSEobj@FM / array(MSEobj@RefPoint$FMSY[,,MSEobj@nyears],dim(MSEobj@SSB))
  #Brel0 = MSEobj@SB_SBMSY 
  #Frel0 = MSEobj@F_FMSY
  
  tt <- Brel > 1 & Frel < 1
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
  
}
class(PGK_stat) <- 'PM'

Yrel = function (MSEobj = NULL, Ref = 1, Yrs = c(1,50))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Yrel: Yield relative to yield in year 0 over Years 1-50"
  PMobj@Caption <- "Yield relative to year 0"
  
  PMobj@Ref <- Ref
  yrel = MSEobj@Catch / array(MSEobj@CB_hist[,MSEobj@nyears],dim(MSEobj@Catch))
 
  tt <- yrel
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob) # same as apply(yrel,2,mean)
  PMobj@MPs <- MSEobj@MPs
  PMobj
  
}
class(Yrel) <- 'PM'
