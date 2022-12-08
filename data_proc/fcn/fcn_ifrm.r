# /----------------------------------------------------------------------------#
#/       create function that tests if object exists                      ------


exist <- function(x) { return(exists(deparse(substitute(x))))}

# if exist remove
ifrm <- function(obj, env = globalenv()) {
  obj <- deparse(substitute(obj))
  if(exists(obj, envir = env)) { rm(list = obj, envir = env) }
}
