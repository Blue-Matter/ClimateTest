
eval_tune = function (MPobj, Hist_list, minfunc){
  MPtest = MPobj
  sfExport("MPtest")
  MSE_list = snowfall::sfLapply(Hist_list, function(X) Project(X, MPs = "MPtest"))
  minfunc(MSE_list)
}


CT_tune = function(OM_list, MPs, type = "SSB", horizon = 20, MP_par_nams = NA, MP_par_intervals = NA ,near_enough = 1E-4, tol = 0.005 ){

  sfExport(list = MPs)

  # Spool-up
  Hist_list = sfLapply(OM_list, Simulate)

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
    cat(paste0("Initial tuning parameter values for MPs (",paste(MPs,collapse=", "),") are (", paste0(round(parinit,3),collapse=", "),") \n"))
    MP_par_intervals = lapply(parinit,function(x)c(x/3, 3*x))
    names(MP_par_intervals) = paste0(MPs,"-",MP_par_nams)
    cat("Tuning parameter ranges are: \n")
    print(MP_par_intervals)
  }

  if(type == "SSB") minfunc = min_cur_SSB
  formals(minfunc)$horizon = horizon

  tuned_MP_list = list()
  MPnames = paste0(MPs,"_tuned")

  for(mp in 1:nMP){
    cat(paste0("--- Tuning MP ",mp,"/",nMP,": ",MPs[mp]," ---------------- \n"))
    tuned_MP_list[[mp]] = tune_MP(Hist_list, MP = MPs[mp], MP_parname = MP_par_nams[mp], interval = MP_par_intervals[[mp]], minfunc, tol=tol, parallel=T)
    #assign(MPnames[mp], tuned_MP_list[[mp]])
  }
  names(tuned_MP_list) = MPnames

  # Final evaluation near zero
  objs = rep(NA,nMP)
  for(mp in 1:nMP){
    par = formals(tuned_MP_list[[mp]])[MP_par_nams[mp]]
    objs[mp] <- eval_tune(tuned_MP_list[[mp]], Hist_list, minfunc)
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

