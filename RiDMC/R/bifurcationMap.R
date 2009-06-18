BifurcationMap <- function(idmc_model,
                           par, var,
                           which.par.x=1, par.x.range, par.x.howMany=100,
                           which.par.y=2, par.y.range, par.y.howMany=100,
                           transient=100, max.period=50,
                           eps=1e-3) {
  m <- idmc_model
  parNames <- getModelParNames(m)
  par <- .sanitizeNamedVector(par, getModelParNames(m), default.value=0)
  checkModelParVar(idmc_model, par, var, txt=deparse(substitute(idmc_model)))
  var <- .sanitizeNamedVector(var, getModelVarNames(m))
  par.x.values <- seq(par.x.range[1], par.x.range[2], length=par.x.howMany)
  par.y.values <- seq(par.y.range[1], par.y.range[2], length=par.y.howMany)
  values <- .Call('ridmc_bifurcation_map', m$model,
                  as.double(par), as.double(var),
                  as.integer(match(c(which.par.x, which.par.y), parNames) - 1),
                  as.double(par.x.values),
                  as.double(par.y.values),
                  as.integer(transient),
                  as.integer(max.period + 1),
                  as.double(eps),
                  PACKAGE='RiDMC')
  ans <- list()

  names(parNames) <- parNames
  which.par.x <- parNames[which.par.x]
  which.par.y <- parNames[which.par.y]
  ans$model <- m
  values <- matrix(values,
                   ncol=par.x.howMany,
                   nrow=par.y.howMany)
  ans$values <- Raster(xlim=par.x.range, ylim=par.y.range,
                       xres=par.x.howMany,
                       yres=par.y.howMany,
                       data=values,
                       xName=which.par.x,
                       yName=which.par.y)
  ans$which.par.x <- which.par.x
  ans$which.par.y <- which.par.y
  ans$max.period <- max.period
  ans$eps <- eps
  class(ans) <- 'idmc_bifurcation_map'
  return(ans)
}

as.matrix.idmc_bifurcation_map <- function(x, ...){
  return(as.matrix(x$values))
}

print.idmc_bifurcation_map <- function(x, ...) {
  cat('= iDMC bifurcation map =\n')
  cat('Model: ', getModelName(x$model), '\n')
  cat('varying parameters: (', x$which.par.x, ';', x$which.par.y, ')\n')
  cat('resolution: ', rasterXres(x$values), 'x', rasterYres(x$values), '\n')
}

plot.idmc_bifurcation_map <- function(x, y, ...) {
  plot(x$values, ...)
}
