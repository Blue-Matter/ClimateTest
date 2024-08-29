
PGK <- function (MSEobj = NULL, Ref = 1, Yrs = c(1,50))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-50"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (years 1-50)"
  
  PMobj@Ref <- Ref
  tt <- MSEobj@SB_SBMSY > 1 & MSEobj@F_FMSY < 1
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
  
}
class(PGK) <- 'PM'