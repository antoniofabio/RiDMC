Basin <- function(model, parms, xlim, xres=100, ylim, 
	yres=100, attractorLimit, attractorIterations) {
	checkModelParVar(model, parms)
	checkPositiveScalar(xres)
	checkPositiveScalar(yres)
	checkPositiveScalar(attractorLimit)
	checkPositiveScalar(attractorIterations)
	if(getModelType(model)!="D")
		stop("'model' should be a discrete model")
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
	basin <- .Call("ridmc_basin_alloc", model$model, as.double(parms),
		as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
		as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
		as.integer(attractorLimit), as.integer(attractorIterations) , 
		PACKAGE='RiDMC')
	while(.Call("ridmc_basin_finished", basin, PACKAGE='RiDMC')==0)
		.Call("ridmc_basin_step", basin, PACKAGE='RiDMC')
	ans$data <- .Call("ridmc_basin_getData", basin, PACKAGE='RiDMC')
	class(ans) <- "idmc_basin"
	return(ans)
}
getBasinModel <- function(obj, ...)
	obj$model
getBasinData <- function(obj, ...)
	obj$data

print.idmc_basin <- function(x, ...){
	mdl <- getBasinModel(x)
	cat('= iDMC basins of attraction =\n')
	cat('Model: ', getModelName(mdl), '\n')
	cat('x-range: ', paste(x$xlim, collapse=','), '\n')
	cat('y-range: ', paste(x$ylim, collapse=','), '\n')
	cat('resolution: ', x$xres, 'by', x$yres, '\n')
	cat('transient: ', x$attractorLimit, '\n')
	cat('attractor iterations: ', x$attractorIterations, '\n')
}

plot.idmc_basin <- function(x, y, palette=rainbow, xlab, ylab, ...) {
	mat <- getBasinData(x)
	nl <- length(unique(as.vector(mat)))
	nc <- NCOL(mat)
	mdl <- getBasinModel(x)
	if(missing(xlab))
		xlab <- getModelVarNames(mdl)[1]
	if(missing(ylab))
		ylab <- getModelVarNames(mdl)[2]
	image(x=seq(x$xlim[1], x$xlim[2], length=x$xres),
		y=seq(x$ylim[1], x$ylim[2], length=x$yres),
		z=mat[,nc:1], 
		breaks=c(1:(nl+1)-0.5) , col=palette(nl), 
		xlab=xlab, ylab=ylab, ... )
}
