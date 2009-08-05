BifurcationMap <- function(idmc_model,
                           par, var,
                           which.par.x, par.x.range, par.x.howMany=100,
                           which.par.y, par.y.range, par.y.howMany=100,
                           transient=100, max.period=50,
                           eps=1e-3) {
  m <- idmc_model
  parNames <- getModelParNames(m)
  names(parNames) <- parNames
  par <- .sanitizeNamedVector(par, parNames)
  checkModelParVar(idmc_model, par, var, txt=deparse(substitute(idmc_model)))
  if(getModelNPar(m) > 2)
    stopifnot(sum(is.finite(par)) >= (getModelNPar(m) - 2))
  if(missing(which.par.x))
    which.par.x <- which(!is.finite(par))[1]
  if(missing(which.par.y))
    which.par.y <- which(!is.finite(par))[2]
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
.getBifurcationMapLabels <- function(x) {
  labels <- sort(unique(as.vector(as.matrix(x))))
  names(labels) <- labels
  labels[labels>x$max.period] <- "unknown"
  labels
}
as.matrix.idmc_bifurcation_map <- function(x, ...){
  return(as.matrix(x$values))
}

print.idmc_bifurcation_map <- function(x, ...) {
  cat('= iDMC bifurcation map =\n')
  cat('Model: ', getModelName(x$model), '\n')
  print(x$values, labels=.getBifurcationMapLabels(x), ...)
}

plot.idmc_bifurcation_map <- function(x, y, main=getModelName(x$model),
                                      legend=TRUE, palette, ...) {
  allLabels <- .getBifurcationMapLabels(x)
  if(!missing(palette)) {
    plot(x$values, main=main, legend=legend, palette=palette,
         labels=allLabels, ...)
  } else {
    plot(x$values, main=main, legend=legend, labels=allLabels, ...)
  }
}
