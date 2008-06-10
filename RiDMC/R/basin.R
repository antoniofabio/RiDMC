Basin <- function(model, parms, xlim, xres=100, ylim,
  yres=100, attractorLimit, attractorIterations,
  method=c("fast", "slow"), ntries, seed) {
  checkModelParVar(model, parms, txt=deparse(substitute(model)))
  checkPositiveScalar(xres)
  checkPositiveScalar(yres)
  checkPositiveScalar(attractorLimit)
  checkPositiveScalar(attractorIterations)
  checkModelDiscrete(model, deparse(substitute(model)))
  if(length(getModelVarNames(model))!=2)
    stop("'model' should have exactly 2 variables")
  method <- match.arg(method)
  ans <- list()
  ans$model <- model
  ans$xlim <- xlim
  ans$xres <- xres
  ans$ylim <- ylim
  ans$yres <- yres
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
    ans$data <- .Call("ridmc_basin_getData", basin, PACKAGE='RiDMC')
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
    ans$data <- .Call("ridmc_basin_slow_getData", basin, PACKAGE='RiDMC')
  }
	ans$basin <- basin
  class(ans) <- "idmc_basin"
  return(ans)
}
getBasinModel <- function(obj, ...)
  obj$model
as.matrix.idmc_basin <- function(x, ...)
	x$data
.getBasinAttractors <- function(obj, ...) UseMethod(".getBasinAttractors")
.getBasinAttractors.idmc_basin <- function(obj, ...) {
  data <- as.matrix(obj)
  vals <- unique(as.vector(data))
  vals <- vals[vals>1]
  attrCodes <- vals[(vals %% 2) == 0]
  na <- length(attrCodes)
	ans <- list()
	xmin <- obj$xlim[1]
	xrange <- diff(obj$xlim)
	ymin <- obj$ylim[1]
	yrange <- diff(obj$ylim)
	for(i in seq_along(attrCodes)) { ##for each attractor
		acd <- attrCodes[i]
		ids <- which(data==acd, arr.ind=TRUE)
		x <- xrange * ids[,1]/NCOL(data) + xmin
		y <- yrange * (1-ids[,2]/NROW(data)) + ymin
		ans[[i]] <- cbind(x, y)
	}
	return(ans)
}

print.idmc_basin <- function(x, ...){
  mdl <- getBasinModel(x)
  cat('= iDMC basins of attraction =\n')
  cat('Model: ', getModelName(mdl), '\n')
  cat('x-range: ', paste(x$xlim, collapse=', '), '\n')
  cat('y-range: ', paste(x$ylim, collapse=', '), '\n')
  cat('resolution: ', x$xres, 'by', x$yres, '\n')
  cat('transient: ', x$attractorLimit, '\n')
  cat('attractor iterations: ', x$attractorIterations, '\n')
}

makeBasinsPalette <- function(values, color.attractors, color.basins, color.infinity, default.palette=rainbow) {
  values <- unique(values)
  attrCodes <- values[(values %% 2)==0] ##attractor codes
  nl <- length(values) ##number of levels
  na <- length(attrCodes) ##number of attractors
  default.palette <- default.palette(na*2)
  if(missing(color.attractors))
    color.attractors <- default.palette[seq(1, by=2, length=na)]
  if(missing(color.basins))
    color.basins <- default.palette[seq(2, by=2, length=na)]
  if(missing(color.infinity))
    color.infinity <- 'black'
  if(length(color.attractors)<na)
    color.attractors <- c(color.attractors, default.palette[seq(1, by=2, length=na)])
  if(length(color.basins)<na)
    color.attractors <- c(color.attractors, default.palette[seq(2, by=2, length=na)])
  col <- numeric(2*na+1)
  col[1] <- color.infinity
  col[1+seq(1, by=2, length=na)] <- color.attractors[seq_len(na)]
  col[1+seq(2, by=2, length=na)] <- color.basins[seq_len(na)]
  col
}

as.grob.idmc_basin <- function(x, color.attractors, color.basins, 
  color.infinity, ...) {
  mat <- as.matrix(x)
  vals <- unique(as.vector(mat))
  vals <- vals[vals>1]
  attrCodes <- vals[(vals %% 2)==0]
  na <- length(attrCodes)
  mat1 <- matrix(1, NROW(mat), NCOL(mat))
  for(i in seq_along(attrCodes)) {
    mat1[mat==attrCodes[i]] <- (i-1)*2 + 2
    mat1[mat==(attrCodes[i]+1)] <- (i-1)*2 + 3
  }
  nc <- NCOL(mat)
  mat1 <- t(mat1[,nc:1])
  col <- makeBasinsPalette(values=vals, color.attractors, color.basins, color.infinity)
  ans <- imageGrob(matrix(col[as.vector(mat1)], NROW(mat1), NCOL(mat1)),
    xlim=x$xlim, ylim=x$ylim, respect = FALSE, name='image')
}

plot.idmc_basin <- function(x, y, color.attractors, color.basins,
  color.infinity, labels.attr, labels.bas, label.infty='infinity',
  main = getModelName(getBasinModel(x)),
  xlab = getModelVarNames(getBasinModel(x))[1],
  ylab = getModelVarNames(getBasinModel(x))[2],
  axes=TRUE, legend=FALSE, attractorPoints=FALSE,
  pch=16, cex=0.2, add=FALSE, ...) {
  imG <- as.grob(x, color.attractors=color.attractors, color.basins=color.basins, color.infinity=color.infinity)
  data <- as.matrix(x)
	attractors <- .getBasinAttractors(x)
  attrCodes <- seq_along(attractors)*2
	vals <- seq_len(length(attractors)*2+1)[-1]
  na <- length(attrCodes)
  col <- makeBasinsPalette(values=vals, color.attractors, color.basins, color.infinity)
  if(legend) {
    if(missing(labels.attr))
      labels.attr <- paste('attractor', seq_len(na))
    if(missing(labels.bas))
      labels.bas <- paste('basin', seq_len(na))
    if(length(labels.attr)<na || length(labels.bas) < na)
      stop('there are ', na, 'attractors/basins pairs to plot: not enough labels provided')
    labels <- character(na*2+1)
    labels[1] <- label.infty
    labels[1+seq(1, by=2, length=na)] <- labels.attr
    labels[1+seq(2, by=2, length=na)] <- labels.bas
    yl <- unit(0, 'npc')
    xl <- unit(0, 'npc')
    clg <- colorLegendGrob(col, labels, y=yl, x=xl, name='legend')
    rightMargin <- convertWidth(widthDetails(clg), 'lines')
    mar <- c(4,4,4,rightMargin)
    mar[4] <- mar[4]*1.04
  } else
    mar <- NULL
  pG <- plotGrob(imG, axes=axes, main=main, xlab=xlab, ylab=ylab, mar=mar)
  if(!add)
    grid.newpage()
  grid.draw(pG)
  if(legend) {
    downViewport('rightMarginArea')
    grid.draw(clg)
    upViewport(0)
  }
  if(attractorPoints) {
    downViewport('plotArea')
    for(i in seq_along(attractors)) { ##for each attractor
			xx <- attractors[[i]]
      grid.points(xx[,1], xx[,2], pch=pch, size=unit(cex, 'char'),
        default.unit='native', gp=gpar(col=col[i*2]))
    }
    upViewport(0)
  }
  invisible(NULL)
}
