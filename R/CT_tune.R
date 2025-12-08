
# Internal function for evaluating the final tuning of the MP
eval_tune = function (MPobj, Hist_list, minfunc,parallel =T){
  MPtest = MPobj
  if(parallel){
    sfExport("MPtest")
    MSE_list = snowfall::sfLapply(Hist_list, function(X) Project(X, MPs = "MPtest", silent=T))
  }else{
    MSE_list = lapply(Hist_list, function(X) Project(X, MPs = "MPtest",silent=T))
  }
  minfunc(MSE_list)
}


#' Climate Test Step 1: Spool up historical period of operating models and prepare projections to have zero climate impacts.
#'
#' Prior to tuning MPs, it is necessary to take the set of operating models and remove any existing climate impacts.
#'
#' @param OM_list A list of objects of class OM (e.g., BET_1, BET_2, BSH_1, BSH_2)
#' @return A list of the same length of historically reconstructed operating models of class Hist
#' @examples
#' OM_list = list(BET_1,BET_2)       # Create a list of operating models of class 'OM'
#' Hist_list = CT_1_prep(OM_list)    # Step 1 prep operating models
#' @author T. Carruthers
#' @export
CT_1_prep = function(OM_list){
  Hist_list = sfLapply(OM_list, Simulate)
  if(any("multiHist" %in% class(Hist_list[[1]]))){
    for(om in 1:length(Hist_list)){
      for(ss in 1:length(Hist_list[[om]])){
        for(ff in 1:length(Hist_list[[om]][[ss]])){
           Hist_list[[om]][[ss]][[ff]]@Misc$MOM@cpars$control$TAC = "removals"
        }
      }
    }
  }
  lapply(Hist_list, do_all)
}


#' Climate Test Step 2: Tune MPs to a stable biomass outcome.
#'
#' A function that tunes a set of MPs to achieve current biomass levels in a specified number of years.
#'
#' @param Hist_list A list of objects of class Hist produced in the first step by CT_1_prep()
#' @param MPs A vector of character strings that are the names of MPs.
#' @param type Character string. The type of tuning - the default is 'SSB' a stable spawning stock biomass.
#' @param horizon Positive integer. The time horizon (number of projected years) at which the outcome (e.g. SSB) is tuned to be the same as current levels.
#' @param MP_par_names A vector of character strings as long as MPs - the names of the tuning parameters corresponding to each MP. By default the algorithm assumes the tuning parameters are an MP argument named 'tune'.
#' @param MP_par_intervals A list (as long as MPs) of vectors, each vector 2 positions long. Optional. These are the lower and upper bounds of the search for the tuning parameter. The algorithm defaults to 1/3 - 3 x the default tuning parameter in each MP.
#' @param near_enough Positive real number. Defaults to 1E-4. A measure of whether the tuning function got close enough to the target (e.g. stable SSB after 30 years). Expressed in units of the ratio of SSB(horizon) / SSB(current).
#' @param tol Positive real number. the tolerance for convergence of Newton search (optimize). Converged when, between iterations, he tuning parameter changes less than this value.
#' @param parallel Boolean. Should the tuning conduct MSE calculations in parallel (across operating models)
#' @return A list of tuned MP functions renamed x_CT.
#' @examples
#' OM_list = list(BET_1,BET_2)                      # Create a list of operating models of class 'OM'
#' Hist_list = CT_1_prep(OM_list)                   # Step 1 prep operating models
#' MPs_tuned = CT_2_tune(Hist_list, c("Ir","It"))   # Step 2 tune management procedures
#' @author T. Carruthers
#' @export
CT_2_tune = function(Hist_list, MPs, type = "SSB", horizon = 20, MP_par_nams = NA, MP_par_intervals = NA ,near_enough = 1E-4, tol = 0.005, parallel = T){

  if(sfIsRunning())sfExport(list = MPs)

  # Tuning MPs
  nMP = length(MPs)
  if(is.na(MP_par_nams[1])){
    cat(paste0("Assuming tuning parameter argument name 'tune' for all MPs \n"))
    MP_par_nams = rep("tune",nMP)
  }
  invalid_args = sapply(1:nMP,function(x,MPs, MP_par_nams){!MP_par_nams[x]%in%names(formals(get(MPs[x])))},MPs = MPs, MP_par_nams=MP_par_nams)
  if(any(invalid_args))stop(paste0("One or more arguments (",paste(MP_par_nams,collapse=", "),") do not match arguments in the MPs (",paste(MPs,collapse=", "),")"))

  if(is.na(MP_par_intervals[1])){
    cat("Determining tuning parameter search ranges from initial argument values \n")
    parinit = as.numeric(sapply(1:nMP,function(x,MPs,MP_par_nams){formals(get(MPs[x]))[MP_par_nams[x]]},MPs=MPs, MP_par_nams=MP_par_nams))
    cat(paste0("Initial tuning parameter (",paste(MP_par_nams,collapse=", "),") values for MPs (",paste(MPs,collapse=", "),") are (", paste0(round(parinit,3),collapse=", "),") \n"))
    MP_par_intervals = lapply(parinit,function(x)c(x/3, 3*x))
    names(MP_par_intervals) = paste0(MPs,"-",MP_par_nams)
    cat("Tuning parameter ranges are: \n")
    print(MP_par_intervals)
  }

  if(type == "SSB") minfunc = min_cur_SSB
  formals(minfunc)$horizon = horizon

  tuned_MP_list = list()
  MPnames = paste0(MPs,"_CT")

  for(mp in 1:nMP){
    cat(paste0("--- Tuning MP ",mp,"/",nMP,": ",MPs[mp]," ---------------- \n"))
    #parallel = F
    #if(length(Hist_list)>1)parallel=T
    tuned_MP_list[[mp]] = CT_tune_MP(Hist_list, MP = MPs[mp], MP_parname = MP_par_nams[mp], interval = MP_par_intervals[[mp]], minfunc, tol=tol, parallel=parallel)
  }
  names(tuned_MP_list) = MPnames

  # Final evaluation near zero
  objs = rep(NA,nMP)
  for(mp in 1:nMP){
    par = formals(tuned_MP_list[[mp]])[MP_par_nams[mp]]
    objs[mp] <- eval_tune(tuned_MP_list[[mp]], Hist_list, minfunc, parallel = parallel)
  }

  if(any(objs>near_enough)){
    cat("The followin MPs failed to tune to the target (higher objective than near_enough) \n")
    cat("Consider whether objective can be obtained within the specified MP_par_intervals \n")
    cat(MPs[objs>near_enough]); cat("\n")
  }else{
    cat("All MPs tuned successfully")
  }

  tuned_MP_list
}

CT_tune_MP = function (Hist_list, MP, MP_parname, interval, minfunc, tol = 0.01,
                     parallel = F)
{
  opt = optimize(CT_tune_int, interval = interval, MP_parname = MP_parname,
                 MP = MP, Hist_list = Hist_list, minfunc = minfunc, tol = tol,
                 parallel = parallel)
  MPout = get(MP)
  formals(MPout)[MP_parname] = opt$minimum
  class(MPout) = "MP"
  return(MPout)
}

CT_tune_int = function (par, MP_parname, MP, Hist_list, minfunc, parallel)
{
  assign("MPtest", get(MP))
  formals(MPtest)[[MP_parname]] = par
  cat(paste0(MP_parname, " = ", round(par, 6), " \n"))
  class(MPtest) = "MP"
  if (!parallel) {
    MSE_list = lapply(Hist_list, function(X) Project(X, MPs = "MPtest", silent=TRUE))
  }
  else {
    sfExport("MPtest")
    MSE_list = snowfall::sfLapply(Hist_list, function(X)Project(X, MPs = "MPtest", silent=TRUE))
  }
  minfunc(MSE_list)
}
