
# Internal funciton for running the MPs on the various spooled up operating models
CT_perf = function(Hist_list, MPs,test = "K", percs, horizon=20, parallel = T){

  nOM = length(Hist_list)  # number of operating models
  nval = length(percs)   # number of percentage changes to test
  OMs = OM_mod(Hist_list, test, percs, horizon) # a nested list of OMs: OMs[[nOM]][[nval]]
  MSEs = list()

  # for each OM, run the nval scenarios
  # eval = Project(OMs[[1]][[8]],MPs[1]); Splot(eval)
  for(i in 1:nOM) MSEs[[i]] = sfLapply(OMs[[i]],function(X,MPs){Project(X,MPs,silent=T)},MPs=MPs)

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


#' Climate Test Step 3: Run the climate tests of tuned management procedures.
#'
#' Prior to tuning MPs, it is necessary to take the set of operating models and remove any existing climate impacts. Returns a list of the same length of historically reconstructed operating models of class Hist
#'
#' @param Hist_list A list of objects of class Hist produced in the first step by CT_1_prep()
#' @param MPs_tuned A list of tuned MP functions produced in the second step by CT_2_tune()
#' @param nlev Positive integer, the number of levels of each marginal climate test from which to linearly interpolate robustness
#' @param horizon Positive integer. The time horizon (number of projected years) at which the outcome (e.g. SSB) is tuned to be the same as current levels. Should be the same as that used in CT_2_tune()
#' @param tests Named vector of positive real numbers, which tests, and the % extent of each climate test. Default to c(M = 25, R = 50, K = 75, S = 200, C = 75) ie a 25 percent increase in natural mortality rate, a 50 percent decrease in recruitment strength, a 75 percent reduction in somatic growth, a 200 percent increase in spatial catchability, a 75 percent reduction in condition factor).
#' @return A two position list that is (1) a hierarchical list of MSEs (tests then levels of tests) and  (2) a matrix with the the levels of the tests (e.g. the ninc levels of natural mortalty rate from zero to the maximum value)
#' @examples
#' OM_list = list(BET_1,BET_2)                     # Create a list of operating models of class 'OM'
#' Hist_list = CT_1_prep(OM_list)                  # Step 1 prep operating models
#' MPs_tuned = CT_2_tune(Hist_list, c("Ir","It"))   # Step 2 tune management procedures
#' data = CT_3_test(Hist_list, MPs_tuned)          # Step 3 run the tuned MPs under a set of increasing marginal climate tests
#' @author T. Carruthers
#' @export
CT_3_test = function(Hist_list, MPs_tuned, nlev = 8, horizon = 30,
                 tests = c(M = 25, R = 50, K = 75, S = 200, C = 75)){

  ntests = length(tests)
  nMP = length(MPs_tuned)
  MPs = names(MPs_tuned)

  for(mp in 1:nMP)assign(MPs[mp], MPs_tuned[[mp]])
  sfExport(list = MPs)

  Bmetric = Ymetric = list()
  MSEs = list()
  levlist = array(NA,c(ntests,nlev))

  for(tt in 1:ntests){
    test = names(tests)[tt]
    percs = seq(0,tests[tt],length.out = nlev)
    levlist[tt, ] = percs
    MSEs[[tt]] = CT_perf(Hist_list, MPs, test, percs, horizon, parallel = T) # a list of MSE objects ninc long
    cat(paste0(test, " marginal climate test completed (",tt,"/",ntests,") \n"))
  }
  rownames(levlist) = names(tests)

  names(MSEs) = names(tests)
  list(MSEs=MSEs, levlist=levlist)

}

