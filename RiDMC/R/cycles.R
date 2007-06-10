cycles_find <- function(idmc_model, par, var, period, eps, max.iter=100) {
	checkModelParVar(idmc_model, par, var)
	checkPositiveScalar(eps)
	checkPositiveScalar(max.iter)
	period <- as.integer(period)
	if(period<1)
		stop('\'period\' should be an integer >=1')
	.Call("ridmc_cycles_find", idmc_model$model, as.double(par), as.double(var), 
		period, as.double(eps), as.integer(max.iter), PACKAGE='RiDMC')
}
