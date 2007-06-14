BasinSlow <- function(model, parms, xlim, xres=100, ylim, 
	yres=100, attractorLimit, attractorIterations, ntries) {
	checkModelParVar(model, parms)
	checkPositiveScalar(xres)
	checkPositiveScalar(yres)
	checkPositiveScalar(attractorLimit)
	checkPositiveScalar(attractorIterations)
	checkPositiveScalar(ntries)
	checkModelDiscrete(model)
	if(length(getModelVarNames(model))!=2)
		stop("'model' should have exactly 2 variables")
	ans <- list()
	ans$model <- model
	ans$xlim <- xlim
	ans$xres <- xres
	ans$ylim <- ylim
	ans$yres <- yres
	ans$attractorLimit <- attractorLimit
	ans$attractorIterations <- attractorIterations
	ans$ntries <- ntries
	basin <- .Call("ridmc_basin_slow_alloc", model$model, as.double(parms),
		as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
		as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
		as.integer(attractorLimit), as.integer(attractorIterations), 
		as.integer(ntries), PACKAGE='RiDMC')
	while(.Call("ridmc_basin_slow_finished", basin, PACKAGE='RiDMC')==0)
		.Call("ridmc_basin_slow_step", basin, PACKAGE='RiDMC')
	ans$data <- .Call("ridmc_basin_slow_getData", basin, PACKAGE='RiDMC')
	class(ans) <- "idmc_basin_slow"
	return(ans)
}
getBasinSlowModel <- function(obj, ...)
	obj$model
getBasinSlowData <- function(obj, ...)
	obj$data

print.idmc_basin_slow <- function(x, ...){
	print.idmc_basin(x, ...)
}

plot.idmc_basin_slow <- function(x, y, palette=rainbow, ...) {
	plot.idmc_basin(x, y, palette=rainbow, ...)
}
