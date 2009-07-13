Bifurcation <- function(idmc_model, which.var.store=1, par, var, 
  which.par.vary=1, par.min, par.max, par.howMany=100,
  transient=100, max.period=50) {
  m <- idmc_model
  parNames <- getModelParNames(m)
  names(parNames) <- parNames
  varNames <- getModelVarNames(m)
  names(varNames) <- varNames
  par <- .sanitizeNamedVector(par, parNames)
  checkModelParVar(idmc_model, par, var, txt=deparse(substitute(idmc_model)))
  numMissingParms <- sum(!is.finite(par))
  stopifnot(numMissingParms %in% c(0, 1))
  var <- .sanitizeNamedVector(var, varNames)
  par.values <- seq(par.min, par.max, length=par.howMany)
  checkPositiveScalar(max.period)
  if(transient != 0)
    checkPositiveScalar(transient)
  which.var.store <- .mustMatchString(which.var.store, varNames)
  if(numMissingParms > 1) {
    stop("you must fix ", getModelNPar(m), " parameters values")
  } else if(numMissingParms == 1) {
    if(!missing(which.par.vary))
      warning("argument 'which.par.vary' is ignored")
    which.par.vary <- parNames[!is.finite(par)]
  }
  which.par.vary <- .mustMatchString(which.par.vary, parNames)
  values <- .Call('ridmc_bifurcation', m$model, 
    as.integer(which.var.store-1),
    as.double(par), as.double(var), as.integer(which.par.vary-1), 
    as.double(par.values), as.integer(transient), 
    as.integer(max.period), PACKAGE='RiDMC')
  ans <- list()
  ans$values <- values
  ans$par.values <- par.values
  ans$which.par <- parNames[which.par.vary]
  ans$which.var <- varNames[which.var.store]
  ans$max.period <- max.period
  ans$model <- m
  ans$par.range <- c(par.min, par.max)
  ans$par.howMany <- par.howMany
  ans$transient <- transient
  ans$max.period <- max.period
  class(ans) <- 'idmc_bifurcation'
  return(ans)
}

as.matrix.idmc_bifurcation <- function(x, ...){
  y <- x$values
  x1 <- rep(x$par.values, each=x$max.period)
  ans <- cbind(x1, y)
  colnames(ans) <- c(x$which.par, x$which.var)
  ans
}

print.idmc_bifurcation <- function(x, ...) {
  cat('= iDMC bifurcation plot =\n')
  cat('Model: ', getModelName(x$model), '\n')
  cat('varying parameter: ', x$which.par, '\n')
  cat('  parameter range: ', paste(x$par.range, collapse=', '), '\n')
  cat('  resolution: ', diff(x$par.range)/x$par.howMany, '\n')
  cat('monitored variable: ', x$which.var, '\n')
  cat('transient: ', x$transient,'\n')
  cat('max period: ', x$max.period, '\n')
}

as.grob.idmc_bifurcation <- function(x, pch=16, cex=0.2, size=unit(cex, 'char'), ...) {
  xx <- as.matrix(x)
  x1 <- xx[,1]
  y <- xx[,2]
  pG <- pointsGrob(x1, y, pch=pch, size=size, name='points')
  contentsGrob(pG, xlim=range(x1[is.finite(x1)]), ylim=.fixLim(range(y[is.finite(y)])))
}

plot.idmc_bifurcation <- function(x, y, pch=16, cex=0.2,
                                  size=unit(cex, 'char'),
                                  main = getModelName(x$model),
                                  xlab = x$which.par,
                                  ylab = x$which.var,
                                  axes=TRUE, bty=TRUE, mar=NULL, add=FALSE, ...) {
  cG <- as.grob(x, pch=pch, cex=cex, size=size)
  PG <- plotGrob(cG, axes=axes, main=main, xlab=xlab, ylab=ylab, mar=mar,
                 ...)
  if(!add)
    grid.newpage()
  grid.draw(PG)
  invisible(PG)
}
