BasinMulti <- function(model, parms, xlim, xres=100, ylim, yres=100, attractorLimit, attractorIterations,
	startValues, xvar=0, yvar=1, ntries=20, eps=getOption("ts.eps"), seed) {
  checkModelParVar(model, parms, startValues, txt=deparse(substitute(model)))
  checkPositiveScalar(xres)
  checkPositiveScalar(yres)
	checkPositiveScalar(eps)
  checkPositiveScalar(attractorLimit)
  checkPositiveScalar(attractorIterations)
  checkModelDiscrete(model, deparse(substitute(model)))
	dim <- length(getModelVarNames(model))
  if(dim<2)
    stop("'model' should have at least 2 variables")
	xvar <- as.integer(xvar)
	yvar <- as.integer(yvar)
	if(!((xvar >= 0) && (xvar < dim) && (yvar >= 0) && (yvar < dim)))
		stop("'xvar' and 'yvar' must be between 0 and", dim)
	if(missing(startValues))
		startValues <- as.double(rep(0.0, dim))
  ans <- list()
  ans$model <- model
  ans$xlim <- xlim
  ans$xres <- xres
  ans$ylim <- ylim
  ans$yres <- yres
	ans$eps <- eps
	ans$method <- "multi"
  ans$attractorLimit <- attractorLimit
  ans$attractorIterations <- attractorIterations
	ans$startValues <- startValues
	checkPositiveScalar(ntries)
	ans$ntries <- as.integer(ntries)
	basin <- .Call("ridmc_basin_multi_alloc", model$model, as.double(parms),
		as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
		as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
		as.double(eps), as.integer(attractorLimit), as.integer(attractorIterations),
		as.integer(ntries), as.integer(xvar), as.integer(yvar), as.double(startValues), PACKAGE='RiDMC')
	if(!missing(seed))
		.Call("ridmc_basin_multi_setGslRngSeed", basin, as.integer(seed), PACKAGE='RiDMC')
	for(i in seq_len(ntries))
		.Call("ridmc_basin_multi_find_next_attractor", basin, PACKAGE='RiDMC')
	while(.Call("ridmc_basin_multi_finished", basin, PACKAGE='RiDMC')==0)
		.Call("ridmc_basin_multi_step", basin, PACKAGE='RiDMC')
	ans$attractors <- .Call("ridmc_basin_multi_get_attractors", basin)
	ans$data <- .Call("ridmc_basin_multi_getData", basin, PACKAGE='RiDMC')
  class(ans) <- c("idmc_basin_multi", "idmc_basin")
  return(ans)
}

.getBasinAttractors.idmc_basin_multi <- function(obj, ...) {
	return(obj$attractors)
}

print.idmc_basin_multi <- function(x, ...){
  mdl <- getBasinModel(x)
	dim <- length(getModelVarNames(mdl))
  cat('= iDMC basins of attraction slice =\n')
  cat('Model: ', getModelName(mdl), '\n')
  cat('x-range: ', paste(x$xlim, collapse=', '), '\n')
  cat('y-range: ', paste(x$ylim, collapse=', '), '\n')
  cat('resolution: ', x$xres, 'by', x$yres, '\n')
  cat('transient: ', x$attractorLimit, '\n')
  cat('attractor iterations: ', x$attractorIterations, '\n')
	cat('neighborhood window: ', x$eps, '\n')
	if(dim>2) {
		startValues <- x$startValues
		names(startValues) <- getModelVarNames(mdl)
		exclude <- c(x$xvar, x$yvar)
		startValues <- startValues[-exclude]
		cat('(fixed) starting values: ', startValues, '\n')
	}
}
