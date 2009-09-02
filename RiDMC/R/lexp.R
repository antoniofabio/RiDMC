lexp <- function(idmc_model, par, var, time, eps) {
  m <- idmc_model
  checkModelParVar(m, par, var, deparse(substitute(idmc_model)))
  checkPositiveScalar(time)
  par <- .sanitizeNamedVector(par, getModelParNames(m))
  var <- .sanitizeNamedVector(var, getModelVarNames(m))
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
  m <- idmc_model
  parNames <- getModelParNames(m)
  names(parNames) <- parNames
  varNames <- getModelVarNames(m)
  names(varNames) <- varNames
  par <- .sanitizeNamedVector(par, parNames)
  checkModelParVar(idmc_model, par, var, txt=deparse(substitute(idmc_model)))
  numMissingParms <- sum(!is.finite(par))
  if(numMissingParms > 1) {
    stop("you must fix ", getModelNPar(m), " parameters values")
  } else if(numMissingParms == 1) {
    if(!missing(which.par.vary))
      warning("argument 'which.par.vary' is ignored")
    which.par.vary <- parNames[!is.finite(par)]
  }
  which.par.vary <- .mustMatchString(which.par.vary, parNames)
  var <- .sanitizeNamedVector(var, varNames)
  par.values <- seq(par.min, par.max, length=par.howMany)
  checkPositiveScalar(time)
  modelType <- getModelType(idmc_model)
  if(modelType=='C') {
    if(missing(eps)) {
      eps <- getOption('ts.eps')
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
  }
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
  ans$which.par <- parNames[which.par.vary]
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
  mle <- range(Filter(is.finite, apply(x$values, 1, max, na.rm=TRUE)), na.rm=TRUE)
  cat('MLE range: [', paste(format(mle, digits=4), collapse=', '), ']\n')
}

as.grob.idmc_lexp_diagram <- function(x, col, lty, xlim=NULL, ylim = NULL, ...) {
  y <- x$values
  x1 <- x$par.values
  nc <- NCOL(y)
  if(missing(col))
    col <- seq_len(nc)
  if(missing(lty))
    lty <- rep(1,nc)
	if(is.null(xlim))
          xlim <- range(Filter(is.finite, x1))
  if(is.null(ylim))
    ylim <- range(Filter(is.finite, as.vector(y)))
  lines <- list()
  for(j in seq_len(nc))
    lines[[j]] <- linesGrob(x1, y[,j], gp=gpar(col=col[j], lty=lty[j], ...), default.units = "native")
  lines <- do.call(gList, lines)
  cG <- gTree(children=lines, name='lines')
  contentsGrob(cG, xlim=xlim, ylim=.fixLim(ylim))
}

plot.idmc_lexp_diagram <- function(x, y, col, lty,
  main = getModelName(x$model),
  xlab = x$which.par,
  ylab = 'Lyapunov exponent',
	xlim = NULL,
  ylim = NULL,
  mar = NULL,
  axes=TRUE,
  bty=TRUE,
  add=FALSE,
  ...) {
  cG <- as.grob(x, col, lty, xlim = xlim, ylim = ylim, ...)
  pG <- plotGrob(cG, axes=axes, main=main, xlab=xlab, ylab=ylab, mar=mar)
  if(!add)
    grid.newpage()
  grid.draw(pG)
	invisible(pG)
}
