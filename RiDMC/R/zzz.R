.onAttach <- function(libname, pkgname) {
  packageStartupMessage('using idmclib version ', paste(idmclibVersion(), collapse='.'))
}
