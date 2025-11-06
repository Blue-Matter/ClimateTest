
#' What objects of this class are available
#'
#' Generic class finder Finds objects of the specified class in the global environment or the
#' ClimateTest package.
#'
#' @param classy A class of object (character string, e.g. 'In', 'OM', 'MSE', 'MP')
#' @param package Optional. Names(s) of the package to search for object of class `classy`. String
#' Default is the `ClimateTest` package.Always searches the global environment as well.
#' @param msg Print messages?
#' @examples
#' objs("In", msg=FALSE)
#' @author T. Carruthers
#' @seealso \link{avail}
#' @examples
#' OMs <- objs("OM")
#' MPs <- objs("MP")
#' @export
objs = function (classy, package = NULL, msg = TRUE) {
  temp <- try(class(classy), silent = TRUE)
  if (methods::is(temp, "try-error"))
    classy <- deparse(substitute(classy))
  if (temp == "function")
    classy <- deparse(substitute(classy))
  else {
    packages <- c("ClimateTest")
    if (is.null(package)) {
      package <- packages
      pkgs <- search()
      search_package <- paste0("package:", package)
      package <- package[search_package %in% pkgs]
    }
    global_funs <- ls(envir = .GlobalEnv)[vapply(ls(envir = .GlobalEnv),
                                                 getclass.ClimateTest, logical(1), classy = classy)]
    temp <- global_funs

    if ('ClimateTest' %in% package) {
      MSEtool_funs <- getfuncs.ClimateTest('ClimateTest', classy, msg)
      temp <- c(temp, MSEtool_funs)
    }


    packagex <- package[!package %in% packages]
    if (length(packagex) > 0) {
      other <- sapply(1:length(packagex), function(i) get_funcs(packagex[i],
                                                                classy, msg))
      other <- unlist(other)
      temp <- c(temp, other)
    }
    if (length(temp) < 1)
      stop("No objects of class '", classy, "' found",
           call. = FALSE)
    return(unique(temp))
  }
}

getclass.ClimateTest = function (x, classy){
  return(any(class(get(x)) == classy))
}

getfuncs.ClimateTest = function (package, classy, msg){
  pkgs <- search()
  search_package <- paste0("package:", package)
  funs <- NULL
  if (search_package %in% pkgs) {
    if (msg)
      message("Searching for objects of class ", classy,
              " in package: ", package)
    funs <- ls(search_package)[vapply(ls(search_package),
                                      getclass.ClimateTest, logical(1), classy = classy)]
  }
  else {
    stop("Package ", package, " not loaded. Use `library(",
         package, ")`", call. = FALSE)
  }
  funs
}
