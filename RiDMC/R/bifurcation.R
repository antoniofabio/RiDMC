
Bifurcation <- function(idmc_model, which.var.store=1, par, var, 
	which.par.variate=1, par.min, par.max, par.howMany=100, 
	transient=100, max.period=50) {
	checkModelParVar(idmc_model, par, var)
	m <- idmc_model
	par.values <- seq(par.min, par.max, length=par.howMany)
	values <- .Call('ridmc_bifurcation', m$model, 
		as.integer(which.var.store-1),
		as.double(par), as.double(var), as.integer(which.par.variate-1), 
		as.double(par.values), as.integer(transient), 
		as.integer(max.period), PACKAGE='RiDMC')
	ans <- list()
	ans$values <- values
	ans$par.values <- par.values
	ans$which.par <- getModelParNames(m)[which.par.variate]
	ans$which.var <- getModelVarNames(m)[which.var.store]
	ans$max.period <- max.period
	class(ans) <- 'idmc_bifurcation'
	return(ans)
}

plot.idmc_bifurcation <- function(x,y, type='p', pch=16, main, xlab, ylab, ...) {
	y <- x$values
	x1 <- rep(x$par.values, each=x$max.period)
	if(missing(main))
		main <- "Bifurcation diagram"
	if(missing(xlab))
		xlab <- x$which.par
	if(missing(ylab))
		ylab <- x$which.var
	plot(x1, y, type=type, pch=pch, main=main, xlab=xlab, ylab=ylab, ...)
}
