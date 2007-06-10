lexp_ode <- function(idmc_c_model, par, var, time, eps) {
	checkModelContinuous(idmc_c_model)
	checkModelParVar(idmc_c_model, par, var)
	checkPositiveScalar(time)
	checkPositiveScalar(eps)
	.Call("ridmc_lexp_ode", idmc_c_model$model, as.double(par), as.double(var), 
		as.double(time), as.double(eps), PACKAGE='RiDMC')
}

lexp <- function(idmc_d_model, par, var, time) {
	checkModelDiscrete(idmc_d_model)
	checkModelParVar(idmc_d_model, par, var)
	checkPositiveScalar(time)
	.Call("ridmc_lexp", idmc_d_model$model, 
		as.double(par), as.double(var), as.integer(time), PACKAGE='RiDMC')
}

LyapunovExponents <- function(idmc_model, par, var, time, eps,
	which.par.variate=1, par.min, par.max, par.howMany=100) {
	checkModelParVar(idmc_model, par, var)
	checkPositiveScalar(time)
	modelType <- getModelType(idmc_model)
	if(modelType=='C')
		checkPositiveScalar(eps)
	m <- idmc_model
	np <- par.howMany
	nv <- length(var)
	parValues <- seq(par.min, par.max, length=np)
	val <- matrix(,np,nv)
	if(modelType=='D') {
		for(i in seq_along(parValues)) {
			par[which.par.variate] <- parValues[i]
			val[i,] <- .Call("ridmc_lexp", m$model, 
				as.double(par), as.double(var), 
				as.integer(time), PACKAGE='RiDMC')
		}
	} else if(modelType=='C'){
		for(i in seq_along(parValues)) {
			par[which.par.variate] <- parValues[i]
			val[i,] <- .Call("ridmc_lexp_ode", 
				m$model, as.double(par), as.double(var), 
				as.double(time), as.double(eps), PACKAGE='RiDMC')
		}
	} else 
		stop('invalid model type')
	ans <- list()
	ans$values <- val
	ans$par.values <- parValues
	ans$which.par <- getModelParNames(m)[which.par.variate]
	class(ans) <- 'lexp_diagram'
	return(ans)	
}
as.matrix.lexp_diagram <- function(x, ...)
	x$values

plot.lexp_diagram  <- function(x,y, type='l', col, lty, main, xlab, ylab, ylim, ...) {
	y <- x$values
	x1 <- x$par.values
	nc <- NCOL(y)
	if(missing(col))
		col <- 1:nc
	if(missing(lty))
		lty <- rep(1,nc)
	if(missing(main))
		main <- "Lyapunov exponents"
	if(missing(xlab))
		xlab <- x$which.par
	if(missing(ylab))
		ylab <- "value"
	if(missing(ylim))
		ylim <- range(as.vector(y))
	plot(x1, y[,1], type='n', ylim=ylim,
		main=main, xlab=xlab, ylab=ylab, ...)
	for(j in 1:nc)
		lines(x1, y[,j], col=col[j], lty=lty[j], ...)
}
