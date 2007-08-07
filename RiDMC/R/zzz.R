.First.lib <- function(libname, pkgname) {
  library.dynam('RiDMC', 'RiDMC')
  message('loaded idmclib version ', paste(idmclibVersion(), collapse='.'))
}
