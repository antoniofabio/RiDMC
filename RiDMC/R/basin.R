Basin <- function(model, parms, xlim, xres=100, ylim,
                  yres=100, attractorLimit, attractorIterations,
                  method=c("fast", "slow"), ntries, seed) {
  checkModelDiscrete(model, deparse(substitute(model)))
  checkModelParVar(model, parms, txt=deparse(substitute(model)))
  checkPositiveScalar(xres)
  checkPositiveScalar(yres)
  checkPositiveScalar(attractorLimit)
  checkPositiveScalar(attractorIterations)
  if(length(getModelVarNames(model))!=2)
    stop("'model' should have exactly 2 variables")
  method <- match.arg(method)
  ans <- list()
  ans$model <- model
  ans$attractorLimit <- attractorLimit
  ans$attractorIterations <- attractorIterations
  ans$method <- method
  if(method=="fast") {
    basin <- .Call("ridmc_basin_alloc", model$model, as.double(parms),
      as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
      as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
      as.integer(attractorLimit), as.integer(attractorIterations) , 
      PACKAGE='RiDMC')
    while(.Call("ridmc_basin_finished", basin, PACKAGE='RiDMC')==0)
      .Call("ridmc_basin_step", basin, PACKAGE='RiDMC')
    values <- .Call("ridmc_basin_getData", basin, PACKAGE='RiDMC')
  } else {
    if(missing(ntries))
      stop("'ntries' is mandatory with 'slow' method")
    checkPositiveScalar(ntries)
    ans$ntries <- as.integer(ntries)
    basin <- .Call("ridmc_basin_slow_alloc", model$model, as.double(parms),
      as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
      as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
      as.integer(attractorLimit), as.integer(attractorIterations), 
      as.integer(ntries), PACKAGE='RiDMC')
    if(!missing(seed))
      .Call("ridmc_basin_slow_setGslRngSeed", basin, 
        as.integer(seed), PACKAGE='RiDMC')
    while(.Call("ridmc_basin_slow_finished", basin, PACKAGE='RiDMC')==0)
      .Call("ridmc_basin_slow_step", basin, PACKAGE='RiDMC')
    values <- .Call("ridmc_basin_slow_getData", basin, PACKAGE='RiDMC')
  }
  values <- t(values)
  values <- values[rev(seq_len(nrow(values))),]
  ans$raster <- Raster(xlim=xlim, ylim=ylim, xres=xres, yres=yres,
                       data=values,
                       xName=getModelVarNames(model)[1],
                       yName=getModelVarNames(model)[2])
  class(ans) <- "idmc_basin"
  return(ans)
}
getBasinModel <- function(obj, ...)
  obj$model
as.matrix.idmc_basin <- function(x, ...)
  as.matrix(x$raster)
.getBasinAttractors <- function(obj, ...) UseMethod(".getBasinAttractors")
.getBasinAttractors.idmc_basin <- function(obj, ...) {
  vals <- unique(as.vector(as.matrix(obj)))
  vals <- vals[vals>1]
  attrCodes <- vals[(vals %% 2) == 0]
  na <- length(attrCodes)
  ans <- list()
  for(i in seq_along(attrCodes)) { ##for each attractor
    ans[[i]] <- raster2Pts(obj$raster, value=attrCodes[i])
  }
  return(ans)
}

print.idmc_basin <- function(x, ...){
  mdl <- getBasinModel(x)
  cat('= iDMC basins of attraction =\n')
  cat('Model: ', getModelName(mdl), '\n')
  cat('x-range: ', paste(rasterXlim(x$raster), collapse=', '), '\n')
  cat('y-range: ', paste(rasterYlim(x$raster), collapse=', '), '\n')
  cat('resolution: ', rasterXres(x$raster), 'by', rasterYres(x$raster), '\n')
  cat('transient: ', x$attractorLimit, '\n')
  cat('attractor iterations: ', x$attractorIterations, '\n')
}

makeBasinsPalette <- function(obj,
                              color.attractors=NULL,
                              color.basins=NULL,
                              color.infinity='black',
                              default.palette=rainbow) {
  values <- unique(as.vector(as.matrix(obj)))
  attrCodes <- values[(values %% 2)==0] ##attractor codes
  nl <- length(values) ##number of levels
  na <- length(attrCodes) ##number of attractors
  default.palette <- default.palette(na*2)
  if(is.null(color.attractors))
    color.attractors <- default.palette[seq(1, by=2, length=na)]
  if(is.null(color.basins))
    color.basins <- default.palette[seq(2, by=2, length=na)]
  if(length(color.attractors)<na)
    color.attractors <- c(color.attractors, default.palette[seq(1, by=2, length=na)])
  if(length(color.basins)<na)
    color.basins <- c(color.basins, default.palette[seq(2, by=2, length=na)])
  col <- numeric(2*na+1)
  col[1] <- color.infinity
  col[1+seq(1, by=2, length=na)] <- color.attractors[seq_len(na)]
  col[1+seq(2, by=2, length=na)] <- color.basins[seq_len(na)]
  names(col) <- seq_along(col)
  return(col)
}

makeBasinsLabels <- function(obj,
                             labels.attractors=NULL,
                             labels.basins=NULL,
                             label.infinity="infinity") {
  na <- length(.getBasinAttractors(obj))
  if(is.null(labels.attractors))
    labels.attractors <- paste('attractor', seq_len(na))
  if(is.null(labels.basins))
    labels.basins <- paste('basin', seq_len(na))
  if(length(labels.attractors)<na || length(labels.basins) < na)
    stop('there are ', na, 'attractors/basins pairs to plot: not enough labels provided')
  labels <- character(na*2+1)
  labels[1] <- label.infinity
  labels[1+seq(1, by=2, length=na)] <- labels.attractors
  labels[1+seq(2, by=2, length=na)] <- labels.basins
  names(labels) <- seq_along(labels)
  return(labels)
}

as.grob.idmc_basin <- function(x, palette, labels, ...) {
  return(rasterGrob(x$raster,
                    palette=palette,
                    labels=labels,
                    ...))
}

plot.idmc_basin <- function(x, y,
                            color.attractors=NULL, color.basins=NULL, color.infinity='black',
                            default.palette=rainbow,
                            labels.attractors=NULL, labels.basins=NULL, label.infinity='infinity',
                            main = getModelName(getBasinModel(x)),
                            legend = TRUE, attractorPoints=FALSE,
                            pch=16, cex=0.2, add=FALSE, ...) {
  palette <- makeBasinsPalette(x,
                               color.attractors=color.attractors,
                               color.basins=color.basins,
                               color.infinity=color.infinity,
                               default.palette=default.palette)
  labels <- makeBasinsLabels(x,
                             labels.attractors=labels.attractors,
                             labels.basins=labels.basins,
                             label.infinity=label.infinity)
  pG <- as.grob(x, palette=palette, labels=labels,
                main=main, legend=legend, ...)
  if(!add)
    grid.newpage()
  grid.draw(pG)
  if(attractorPoints) {
    depth <- downViewport('plotArea')
    attractors <- .getBasinAttractors(x)
    for(i in seq_along(attractors)) { ##for each attractor
      xx <- attractors[[i]]
      grid.points(xx[,1], xx[,2], pch=pch, size=unit(cex, 'char'),
                  default.unit='native', gp=gpar(col=palette[i*2]))
    }
    upViewport(depth)
  }
  invisible(pG)
}
