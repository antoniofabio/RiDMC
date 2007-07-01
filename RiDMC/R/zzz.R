.First.lib <- function(libname, pkgname) {
	library.dynam('RiDMC', 'RiDMC')
	vrs <- idmclibVersion()
	message('loaded idmclib version ', vrs[1],'.',vrs[2],'.',vrs[3])
}
