lexp <- function(idmc_model, par, var, time, eps) {
  m <- idmc_model
  checkModelParVar(m, par, var)
  checkPositiveScalar(time)
  if(getModelType(m)=='C') {
    if(missing(eps)) {
      eps <- getOption('ts.eps')
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
    ans <- .Call("ridmc_lexp_ode", m$model, as.double(par), as.double(var), 
      as.double(time), as.double(eps), PACKAGE='RiDMC')
  } else {
    ans <- .Call("ridmc_lexp", m$model, 
      as.double(par), as.double(var), as.integer(time), PACKAGE='RiDMC')
  }
  return(ans)
}

LyapunovExponents <- function(idmc_model, par, var, time, eps,
  which.par.vary=1, par.min, par.max, par.howMany=100) {
  checkModelParVar(idmc_model, par, var)
  checkPositiveScalar(time)
  modelType <- getModelType(idmc_model)
  if(modelType=='C') {
    if(missing(eps)) {
      eps <- getOption('ts.eps')
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
  }
  m <- idmc_model
  np <- par.howMany
  nv <- length(var)
  parValues <- seq(par.min, par.max, length=np)
  val <- matrix(,np,nv)
  if(modelType=='D') {
    for(i in seq_along(parValues)) {
      par[which.par.vary] <- parValues[i]
      val[i,] <- .Call("ridmc_lexp", m$model,
        as.double(par), as.double(var),
        as.integer(time), PACKAGE='RiDMC')
    }
  } else if(modelType=='C'){
    for(i in seq_along(parValues)) {
      par[which.par.vary] <- parValues[i]
      val[i,] <- .Call("ridmc_lexp_ode", 
        m$model, as.double(par), as.double(var), 
        as.double(time), as.double(eps), PACKAGE='RiDMC')
    }
  } else 
    stop('invalid model type')
  ans <- list()
  ans$model <- idmc_model
  ans$var <- var
  ans$par <- par
  ans$values <- val
  ans$par.values <- parValues
  ans$which.par <- getModelParNames(m)[which.par.vary]
  class(ans) <- 'idmc_lexp_diagram'
  return(ans)
}
as.matrix.idmc_lexp_diagram <- function(x, ...)
  x$values

print.idmc_lexp_diagram <- function(x, ...) {
  m <- x$model
  cat('=iDMC Lyapunov exponents diagram=\n')
  cat('Model: ', getModelName(m), '\n')
  cat('Starting point: ')
    tmp <- getModelVarNames(m)
    cat(paste(tmp, x$var, sep=' = ', collapse=', '), '\n')
  cat('Parameter values: ')
    tmp <- getModelParNames(m)
    cat(paste(tmp, x$par, sep=' = ', collapse=', '), '\n')
  cat('Varying par.: ', x$which.par, '\n')
  cat('Varying par. range: [', paste(range(x$par.values), collapse=', '), ']\n')
  mle <- range(apply(x$values, 1, max, na.rm=TRUE), na.rm=TRUE)
  cat('MLE range: [', paste(format(mle, digits=4), collapse=', '), ']\n')
}

plot.idmc_lexp_diagram  <- function(x,y, type='l', col, lty, main, xlab, ylab, ylim, ...) {
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
