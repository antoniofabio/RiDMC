idmclibVersion <- function() {
	ans <- .Call("ridmc_version", PACKAGE='RiDMC')
	names(ans) <- c("major","minor","micro")
	ans
}
